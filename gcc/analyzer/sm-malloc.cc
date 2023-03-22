/* A state machine for detecting misuses of the malloc/free API.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "bitmap.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/call-details.h"
#include "stringpool.h"
#include "attribs.h"
#include "analyzer/function-set.h"
#include "analyzer/program-state.h"
#include "analyzer/checker-event.h"
#include "analyzer/exploded-graph.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* This state machine and its various support classes track allocations
   and deallocations.

   It has a few standard allocation/deallocation pairs (e.g. new/delete),
   and also supports user-defined ones via
   __attribute__ ((malloc(DEALLOCATOR))).

   There can be more than one valid deallocator for a given allocator,
   for example:
     __attribute__ ((malloc (fclose)))
     __attribute__ ((malloc (freopen, 3)))
     FILE* fopen (const char*, const char*);
   A deallocator_set represents a particular set of valid deallocators.

   We track the expected deallocator_set for a value, but not the allocation
   function - there could be more than one allocator per deallocator_set.
   For example, there could be dozens of allocators for "free" beyond just
   malloc e.g. calloc, xstrdup, etc.  We don't want to explode the number
   of states by tracking individual allocators in the exploded graph;
   we merely want to track "this value expects to have 'free' called on it".
   Perhaps we can reconstruct which allocator was used later, when emitting
   the path, if it's necessary for precision of wording of diagnostics.  */

class deallocator;
class deallocator_set;
class malloc_state_machine;

/* An enum for discriminating between different kinds of allocation_state.  */

enum resource_state
{
  /* States that are independent of allocator/deallocator.  */

  /* The start state.  */
  RS_START,

  /* State for a pointer that's been unconditionally dereferenced.  */
  RS_ASSUMED_NON_NULL,

  /* State for a pointer that's known to be NULL.  */
  RS_NULL,

  /* State for a pointer that's known to not be on the heap (e.g. to a local
     or global).  */
  RS_NON_HEAP,

  /* Stop state, for pointers we don't want to track any more.  */
  RS_STOP,

  /* States that relate to a specific deallocator_set.  */

  /* State for a pointer returned from an allocator that hasn't
     been checked for NULL.
     It could be a pointer to heap-allocated memory, or could be NULL.  */
  RS_UNCHECKED,

  /* State for a pointer returned from an allocator,
     known to be non-NULL.  */
  RS_NONNULL,

  /* State for a pointer passed to a deallocator.  */
  RS_FREED
};

/* Custom state subclass, which can optionally refer to an a
   deallocator_set.  */

struct allocation_state : public state_machine::state
{
  allocation_state (const char *name, unsigned id,
		    enum resource_state rs,
		    const deallocator_set *deallocators,
		    const deallocator *deallocator)
  : state (name, id), m_rs (rs),
    m_deallocators (deallocators),
    m_deallocator (deallocator)
  {}

  void dump_to_pp (pretty_printer *pp) const override;

  const allocation_state *get_nonnull () const;

  enum resource_state m_rs;
  const deallocator_set *m_deallocators;
  const deallocator *m_deallocator;
};

/* Custom state subclass, for the "assumed-non-null" state
   where the assumption happens in a particular frame.  */

struct assumed_non_null_state : public allocation_state
{
  assumed_non_null_state (const char *name, unsigned id,
			  const frame_region *frame)
  : allocation_state (name, id, RS_ASSUMED_NON_NULL,
		      NULL, NULL),
    m_frame (frame)
  {
    gcc_assert (m_frame);
  }

  void dump_to_pp (pretty_printer *pp) const final override;

  const frame_region *m_frame;
};

/* An enum for choosing which wording to use in various diagnostics
   when describing deallocations.  */

enum wording
{
  WORDING_FREED,
  WORDING_DELETED,
  WORDING_DEALLOCATED,
  WORDING_REALLOCATED
};

/* Base class representing a deallocation function,
   either a built-in one we know about, or one exposed via
   __attribute__((malloc(DEALLOCATOR))).  */

struct deallocator
{
  hashval_t hash () const;
  void dump_to_pp (pretty_printer *pp) const;
  static int cmp (const deallocator *a, const deallocator *b);
  static int cmp_ptr_ptr (const void *, const void *);

  /* Name to use in diagnostics.  */
  const char *m_name;

  /* Which wording to use in diagnostics.  */
  enum wording m_wording;

  /* State for a value passed to one of the deallocators.  */
  state_machine::state_t m_freed;

protected:
  deallocator (malloc_state_machine *sm,
	       const char *name,
	       enum wording wording);
};

/* Subclass representing a predefined deallocator.
   e.g. "delete []", without needing a specific FUNCTION_DECL
   ahead of time.  */

struct standard_deallocator : public deallocator
{
  standard_deallocator (malloc_state_machine *sm,
			const char *name,
			enum wording wording);
};

/* Subclass representing a user-defined deallocator
   via __attribute__((malloc(DEALLOCATOR))) given
   a specific FUNCTION_DECL.  */

struct custom_deallocator : public deallocator
{
  custom_deallocator (malloc_state_machine *sm,
		      tree deallocator_fndecl,
		      enum wording wording)
  : deallocator (sm, IDENTIFIER_POINTER (DECL_NAME (deallocator_fndecl)),
		 wording)
  {
  }
};

/* Base class representing a set of possible deallocators.
   Often this will be just a single deallocator, but some
   allocators have multiple valid deallocators (e.g. the result of
   "fopen" can be closed by either "fclose" or "freopen").  */

struct deallocator_set
{
  deallocator_set (malloc_state_machine *sm,
		   enum wording wording);
  virtual ~deallocator_set () {}

  virtual bool contains_p (const deallocator *d) const = 0;
  virtual const deallocator *maybe_get_single () const = 0;
  virtual void dump_to_pp (pretty_printer *pp) const = 0;
  void dump () const;

  /* Which wording to use in diagnostics.  */
  enum wording m_wording;

  /* Pointers to states.
     These states are owned by the state_machine base class.  */

  /* State for an unchecked result from an allocator using this set.  */
  state_machine::state_t m_unchecked;

  /* State for a known non-NULL result from such an allocator.  */
  state_machine::state_t m_nonnull;
};

/* Subclass of deallocator_set representing a set of deallocators
   defined by one or more __attribute__((malloc(DEALLOCATOR))).  */

struct custom_deallocator_set : public deallocator_set
{
  typedef const auto_vec <const deallocator *> *key_t;

  custom_deallocator_set (malloc_state_machine *sm,
			  const auto_vec <const deallocator *> *vec,
			  //const char *name,
			  //const char *dealloc_funcname,
			  //unsigned arg_idx,
			  enum wording wording);

  bool contains_p (const deallocator *d) const final override;
  const deallocator *maybe_get_single () const final override;
  void dump_to_pp (pretty_printer *pp) const final override;

  auto_vec <const deallocator *> m_deallocator_vec;
};

/* Subclass of deallocator_set representing a set of deallocators
   with a single standard_deallocator, e.g. "delete []".  */

struct standard_deallocator_set : public deallocator_set
{
  standard_deallocator_set (malloc_state_machine *sm,
			    const char *name,
			    enum wording wording);

  bool contains_p (const deallocator *d) const final override;
  const deallocator *maybe_get_single () const final override;
  void dump_to_pp (pretty_printer *pp) const final override;

  standard_deallocator m_deallocator;
};

/* Traits class for ensuring uniqueness of deallocator_sets within
   malloc_state_machine.  */

struct deallocator_set_map_traits
{
  typedef custom_deallocator_set::key_t key_type;
  typedef custom_deallocator_set *value_type;
  typedef custom_deallocator_set *compare_type;

  static inline hashval_t hash (const key_type &k)
  {
    gcc_assert (k != NULL);
    gcc_assert (k != reinterpret_cast<key_type> (1));

    hashval_t result = 0;
    unsigned i;
    const deallocator *d;
    FOR_EACH_VEC_ELT (*k, i, d)
      result ^= d->hash ();
    return result;
  }
  static inline bool equal_keys (const key_type &k1, const key_type &k2)
  {
    if (k1->length () != k2->length ())
      return false;

    for (unsigned i = 0; i < k1->length (); i++)
      if ((*k1)[i] != (*k2)[i])
	return false;

    return true;
  }
  template <typename T>
  static inline void remove (T &)
  {
    /* empty; the nodes are handled elsewhere.  */
  }
  template <typename T>
  static inline void mark_deleted (T &entry)
  {
    entry.m_key = reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline void mark_empty (T &entry)
  {
    entry.m_key = NULL;
  }
  template <typename T>
  static inline bool is_deleted (const T &entry)
  {
    return entry.m_key == reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline bool is_empty (const T &entry)
  {
    return entry.m_key == NULL;
  }
  static const bool empty_zero_p = false;
};

/* A state machine for detecting misuses of the malloc/free API.

   See sm-malloc.dot for an overview (keep this in-sync with that file).  */

class malloc_state_machine : public state_machine
{
public:
  typedef allocation_state custom_data_t;

  malloc_state_machine (logger *logger);
  ~malloc_state_machine ();

  state_t
  add_state (const char *name, enum resource_state rs,
	     const deallocator_set *deallocators,
	     const deallocator *deallocator);

  bool inherited_state_p () const final override { return false; }

  state_machine::state_t
  get_default_state (const svalue *sval) const final override
  {
    if (tree cst = sval->maybe_get_constant ())
      {
	if (zerop (cst))
	  return m_null;
      }
    if (const region_svalue *ptr = sval->dyn_cast_region_svalue ())
      {
	const region *reg = ptr->get_pointee ();
	switch (reg->get_memory_space ())
	  {
	  default:
	    break;
	  case MEMSPACE_CODE:
	  case MEMSPACE_GLOBALS:
	  case MEMSPACE_STACK:
	  case MEMSPACE_READONLY_DATA:
	    return m_non_heap;
	  }
      }
    return m_start;
  }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const final override;

  void on_phi (sm_context *sm_ctxt,
	       const supernode *node,
	       const gphi *phi,
	       tree rhs) const final override;

  void on_condition (sm_context *sm_ctxt,
		     const supernode *node,
		     const gimple *stmt,
		     const svalue *lhs,
		     enum tree_code op,
		     const svalue *rhs) const final override;

  void on_pop_frame (sm_state_map *smap,
		     const frame_region *) const final override;

  bool can_purge_p (state_t s) const final override;
  std::unique_ptr<pending_diagnostic> on_leak (tree var) const final override;

  bool reset_when_passed_to_unknown_fn_p (state_t s,
					  bool is_mutable) const final override;

  state_t
  maybe_get_merged_states_nonequal (state_t state_a,
				    state_t state_b) const final override;

  static bool unaffected_by_call_p (tree fndecl);

  void maybe_assume_non_null (sm_context *sm_ctxt,
			      tree ptr,
			      const gimple *stmt) const;

  void on_realloc_with_move (region_model *model,
			     sm_state_map *smap,
			     const svalue *old_ptr_sval,
			     const svalue *new_ptr_sval,
			     const extrinsic_state &ext_state) const;

  standard_deallocator_set m_free;
  standard_deallocator_set m_scalar_delete;
  standard_deallocator_set m_vector_delete;

  standard_deallocator m_realloc;

  /* States that are independent of api.  */

  /* States for a pointer that's been unconditionally dereferenced
     in a particular stack frame.  */
  hash_map<const frame_region *, state_t> m_assumed_non_null;

  /* State for a pointer that's known to be NULL.  */
  state_t m_null;

  /* State for a pointer that's known to not be on the heap (e.g. to a local
     or global).  */
  state_t m_non_heap; // TODO: or should this be a different state machine?
  // or do we need child values etc?

  /* Stop state, for pointers we don't want to track any more.  */
  state_t m_stop;

private:
  const custom_deallocator_set *
  get_or_create_custom_deallocator_set (tree allocator_fndecl);
  custom_deallocator_set *
  maybe_create_custom_deallocator_set (tree allocator_fndecl);
  const deallocator *
  get_or_create_deallocator (tree deallocator_fndecl);

  state_t
  get_or_create_assumed_non_null_state_for_frame (const frame_region *frame);

  void
  maybe_complain_about_deref_before_check (sm_context *sm_ctxt,
					   const supernode *node,
					   const gimple *stmt,
					   const assumed_non_null_state *,
					   tree ptr) const;

  void on_allocator_call (sm_context *sm_ctxt,
			  const gcall *call,
			  const deallocator_set *deallocators,
			  bool returns_nonnull = false) const;
  void handle_free_of_non_heap (sm_context *sm_ctxt,
				const supernode *node,
				const gcall *call,
				tree arg,
				const deallocator *d) const;
  void on_deallocator_call (sm_context *sm_ctxt,
			    const supernode *node,
			    const gcall *call,
			    const deallocator *d,
			    unsigned argno) const;
  void on_realloc_call (sm_context *sm_ctxt,
			const supernode *node,
			const gcall *call) const;
  void on_zero_assignment (sm_context *sm_ctxt,
			   const gimple *stmt,
			   tree lhs) const;

  /* A map for consolidating deallocators so that they are
     unique per deallocator FUNCTION_DECL.  */
  typedef hash_map<tree, deallocator *> deallocator_map_t;
  deallocator_map_t m_deallocator_map;

  /* Memoized lookups from FUNCTION_DECL to custom_deallocator_set *.  */
  typedef hash_map<tree, custom_deallocator_set *> deallocator_set_cache_t;
  deallocator_set_cache_t m_custom_deallocator_set_cache;

  /* A map for consolidating custom_deallocator_set instances.  */
  typedef hash_map<custom_deallocator_set::key_t,
		   custom_deallocator_set *,
		   deallocator_set_map_traits> custom_deallocator_set_map_t;
  custom_deallocator_set_map_t m_custom_deallocator_set_map;

  /* Record of dynamically-allocated objects,  for cleanup.  */
  auto_vec <custom_deallocator_set *> m_dynamic_sets;
  auto_vec <custom_deallocator *> m_dynamic_deallocators;
};

/* struct deallocator.  */

deallocator::deallocator (malloc_state_machine *sm,
			  const char *name,
			  enum wording wording)
: m_name (name),
  m_wording (wording),
  m_freed (sm->add_state ("freed", RS_FREED, NULL, this))
{
}

hashval_t
deallocator::hash () const
{
  return (hashval_t)m_freed->get_id ();
}

void
deallocator::dump_to_pp (pretty_printer *pp) const
{
  pp_printf (pp, "%qs", m_name);
}

int
deallocator::cmp (const deallocator *a, const deallocator *b)
{
  return (int)a->m_freed->get_id () - (int)b->m_freed->get_id ();
}

int
deallocator::cmp_ptr_ptr (const void *a, const void *b)
{
  return cmp (*(const deallocator * const *)a,
	      *(const deallocator * const *)b);
}


/* struct standard_deallocator : public deallocator.  */

standard_deallocator::standard_deallocator (malloc_state_machine *sm,
					    const char *name,
					    enum wording wording)
: deallocator (sm, name, wording)
{
}

/* struct deallocator_set.  */

deallocator_set::deallocator_set (malloc_state_machine *sm,
				  enum wording wording)
: m_wording (wording),
  m_unchecked (sm->add_state ("unchecked", RS_UNCHECKED, this, NULL)),
  m_nonnull (sm->add_state ("nonnull", RS_NONNULL, this, NULL))
{
}

/* Dump a description of this deallocator_set to stderr.  */

DEBUG_FUNCTION void
deallocator_set::dump () const
{
  pretty_printer pp;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* struct custom_deallocator_set : public deallocator_set.  */

custom_deallocator_set::
custom_deallocator_set (malloc_state_machine *sm,
			const auto_vec <const deallocator *> *vec,
			enum wording wording)
: deallocator_set (sm, wording),
  m_deallocator_vec (vec->length ())
{
  unsigned i;
  const deallocator *d;
  FOR_EACH_VEC_ELT (*vec, i, d)
    m_deallocator_vec.safe_push (d);
}

bool
custom_deallocator_set::contains_p (const deallocator *d) const
{
  unsigned i;
  const deallocator *cd;
  FOR_EACH_VEC_ELT (m_deallocator_vec, i, cd)
    if (cd == d)
      return true;
  return false;
}

const deallocator *
custom_deallocator_set::maybe_get_single () const
{
  if (m_deallocator_vec.length () == 1)
    return m_deallocator_vec[0];
  return NULL;
}

void
custom_deallocator_set::dump_to_pp (pretty_printer *pp) const
{
  pp_character (pp, '{');
  unsigned i;
  const deallocator *d;
  FOR_EACH_VEC_ELT (m_deallocator_vec, i, d)
    {
      if (i > 0)
	pp_string (pp, ", ");
      d->dump_to_pp (pp);
    }
  pp_character (pp, '}');
}

/* struct standard_deallocator_set : public deallocator_set.  */

standard_deallocator_set::standard_deallocator_set (malloc_state_machine *sm,
						    const char *name,
						    enum wording wording)
: deallocator_set (sm, wording),
  m_deallocator (sm, name, wording)
{
}

bool
standard_deallocator_set::contains_p (const deallocator *d) const
{
  return d == &m_deallocator;
}

const deallocator *
standard_deallocator_set::maybe_get_single () const
{
  return &m_deallocator;
}

void
standard_deallocator_set::dump_to_pp (pretty_printer *pp) const
{
  pp_character (pp, '{');
  pp_string (pp, m_deallocator.m_name);
  pp_character (pp, '}');
}

/* Return STATE cast to the custom state subclass, or NULL for the start state.
   Everything should be an allocation_state apart from the start state.  */

static const allocation_state *
dyn_cast_allocation_state (state_machine::state_t state)
{
  if (state->get_id () == 0)
    return NULL;
  return static_cast <const allocation_state *> (state);
}

/* Return STATE cast to the custom state subclass, for a state that is
   already known to not be the start state .  */

static const allocation_state *
as_a_allocation_state (state_machine::state_t state)
{
  gcc_assert (state->get_id () != 0);
  return static_cast <const allocation_state *> (state);
}

/* Get the resource_state for STATE.  */

static enum resource_state
get_rs (state_machine::state_t state)
{
  if (const allocation_state *astate = dyn_cast_allocation_state (state))
    return astate->m_rs;
  else
    return RS_START;
}

/* Return true if STATE is the start state.  */

static bool
start_p (state_machine::state_t state)
{
  return get_rs (state) == RS_START;
}

/* Return true if STATE is an unchecked result from an allocator.  */

static bool
unchecked_p (state_machine::state_t state)
{
  return get_rs (state) == RS_UNCHECKED;
}

/* Return true if STATE is a non-null result from an allocator.  */

static bool
nonnull_p (state_machine::state_t state)
{
  return get_rs (state) == RS_NONNULL;
}

/* Return true if STATE is a value that has been passed to a deallocator.  */

static bool
freed_p (state_machine::state_t state)
{
  return get_rs (state) == RS_FREED;
}

/* Return true if STATE is a value that has been assumed to be non-NULL.  */

static bool
assumed_non_null_p (state_machine::state_t state)
{
  return get_rs (state) == RS_ASSUMED_NON_NULL;
}

/* Class for diagnostics relating to malloc_state_machine.  */

class malloc_diagnostic : public pending_diagnostic
{
public:
  malloc_diagnostic (const malloc_state_machine &sm, tree arg)
  : m_sm (sm), m_arg (arg)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    return same_tree_p (m_arg, ((const malloc_diagnostic &)base_other).m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& unchecked_p (change.m_new_state))
      // TODO: verify that it's the allocation stmt, not a copy
      return label_text::borrow ("allocated here");
    if (unchecked_p (change.m_old_state)
	&& nonnull_p (change.m_new_state))
      {
	if (change.m_expr)
	  return change.formatted_print ("assuming %qE is non-NULL",
					 change.m_expr);
	else
	  return change.formatted_print ("assuming %qs is non-NULL",
					 "<unknown>");
      }
    if (change.m_new_state == m_sm.m_null)
      {
	if (unchecked_p (change.m_old_state))
	  {
	    if (change.m_expr)
	      return change.formatted_print ("assuming %qE is NULL",
					     change.m_expr);
	    else
	      return change.formatted_print ("assuming %qs is NULL",
					     "<unknown>");
	  }
	else
	  {
	    if (change.m_expr)
	      return change.formatted_print ("%qE is NULL",
					     change.m_expr);
	    else
	      return change.formatted_print ("%qs is NULL",
					     "<unknown>");
	  }
      }

    return label_text ();
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (const evdesc::state_change &change)
    const final override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& unchecked_p (change.m_new_state))
      return diagnostic_event::meaning (diagnostic_event::VERB_acquire,
					diagnostic_event::NOUN_memory);
    if (freed_p (change.m_new_state))
      return diagnostic_event::meaning (diagnostic_event::VERB_release,
					diagnostic_event::NOUN_memory);
    return diagnostic_event::meaning ();
  }

protected:
  const malloc_state_machine &m_sm;
  tree m_arg;
};

/* Concrete subclass for reporting mismatching allocator/deallocator
   diagnostics.  */

class mismatching_deallocation : public malloc_diagnostic
{
public:
  mismatching_deallocation (const malloc_state_machine &sm, tree arg,
			    const deallocator_set *expected_deallocators,
			    const deallocator *actual_dealloc)
  : malloc_diagnostic (sm, arg),
    m_expected_deallocators (expected_deallocators),
    m_actual_dealloc (actual_dealloc)
  {}

  const char *get_kind () const final override
  {
    return "mismatching_deallocation";
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_mismatching_deallocation;
  }

  bool emit (rich_location *rich_loc) final override
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (762); /* CWE-762: Mismatched Memory Management Routines.  */
    if (const deallocator *expected_dealloc
	  = m_expected_deallocators->maybe_get_single ())
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "%qE should have been deallocated with %qs"
			   " but was deallocated with %qs",
			   m_arg, expected_dealloc->m_name,
			   m_actual_dealloc->m_name);
    else
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "%qs called on %qE returned from a mismatched"
			   " allocation function",
			   m_actual_dealloc->m_name, m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (unchecked_p (change.m_new_state))
      {
	m_alloc_event = change.m_event_id;
	if (const deallocator *expected_dealloc
	    = m_expected_deallocators->maybe_get_single ())
	  return change.formatted_print ("allocated here"
					 " (expects deallocation with %qs)",
					 expected_dealloc->m_name);
	else
	  return change.formatted_print ("allocated here");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_alloc_event.known_p ())
      {
	if (const deallocator *expected_dealloc
	    = m_expected_deallocators->maybe_get_single ())
	  return ev.formatted_print
	    ("deallocated with %qs here;"
	     " allocation at %@ expects deallocation with %qs",
	     m_actual_dealloc->m_name, &m_alloc_event,
	     expected_dealloc->m_name);
	else
	  return ev.formatted_print
	    ("deallocated with %qs here;"
	     " allocated at %@",
	     m_actual_dealloc->m_name, &m_alloc_event);
      }
    return ev.formatted_print ("deallocated with %qs here",
			       m_actual_dealloc->m_name);
  }

private:
  diagnostic_event_id_t m_alloc_event;
  const deallocator_set *m_expected_deallocators;
  const deallocator *m_actual_dealloc;
};

/* Concrete subclass for reporting double-free diagnostics.  */

class double_free : public malloc_diagnostic
{
public:
  double_free (const malloc_state_machine &sm, tree arg, const char *funcname)
  : malloc_diagnostic (sm, arg), m_funcname (funcname)
  {}

  const char *get_kind () const final override { return "double_free"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_double_free;
  }

  bool emit (rich_location *rich_loc) final override
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (415); /* CWE-415: Double Free.  */
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "double-%qs of %qE", m_funcname, m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (freed_p (change.m_new_state))
      {
	m_first_free_event = change.m_event_id;
	return change.formatted_print ("first %qs here", m_funcname);
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_call_with_state (const evdesc::call_with_state &info)
    final override
  {
    if (freed_p (info.m_state))
      return info.formatted_print
	("passing freed pointer %qE in call to %qE from %qE",
	 info.m_expr, info.m_callee_fndecl, info.m_caller_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_first_free_event.known_p ())
      return ev.formatted_print ("second %qs here; first %qs was at %@",
				 m_funcname, m_funcname,
				 &m_first_free_event);
    return ev.formatted_print ("second %qs here", m_funcname);
  }

private:
  diagnostic_event_id_t m_first_free_event;
  const char *m_funcname;
};

/* Abstract subclass for describing possible bad uses of NULL.
   Responsible for describing the call that could return NULL.  */

class possible_null : public malloc_diagnostic
{
public:
  possible_null (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg)
  {}

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& unchecked_p (change.m_new_state))
      {
	m_origin_of_unchecked_event = change.m_event_id;
	return label_text::borrow ("this call could return NULL");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    final override
  {
    if (unchecked_p (info.m_state))
      return info.formatted_print ("possible return of NULL to %qE from %qE",
				   info.m_caller_fndecl, info.m_callee_fndecl);
    return label_text ();
  }

protected:
  diagnostic_event_id_t m_origin_of_unchecked_event;
};

/* Concrete subclass for describing dereference of a possible NULL
   value.  */

class possible_null_deref : public possible_null
{
public:
  possible_null_deref (const malloc_state_machine &sm, tree arg)
  : possible_null (sm, arg)
  {}

  const char *get_kind () const final override { return "possible_null_deref"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_possible_null_dereference;
  }

  bool emit (rich_location *rich_loc) final override
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    diagnostic_metadata m;
    m.add_cwe (690);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "dereference of possibly-NULL %qE", m_arg);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_origin_of_unchecked_event.known_p ())
      return ev.formatted_print ("%qE could be NULL: unchecked value from %@",
				 ev.m_expr,
				 &m_origin_of_unchecked_event);
    else
      return ev.formatted_print ("%qE could be NULL", ev.m_expr);
  }

};

/* Return true if FNDECL is a C++ method.  */

static bool
method_p (tree fndecl)
{
  return TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE;
}

/* Return a 1-based description of ARG_IDX (0-based) of FNDECL.
   Compare with %P in the C++ FE  (implemented in cp/error.cc: parm_to_string
   as called from cp_printer).  */

static label_text
describe_argument_index (tree fndecl, int arg_idx)
{
  if (method_p (fndecl))
    if (arg_idx == 0)
      return label_text::borrow ("'this'");
  pretty_printer pp;
  pp_printf (&pp, "%u", arg_idx + 1 - method_p (fndecl));
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* Subroutine for use by possible_null_arg::emit and null_arg::emit.
   Issue a note informing that the pertinent argument must be non-NULL.  */

static void
inform_nonnull_attribute (tree fndecl, int arg_idx)
{
  label_text arg_desc = describe_argument_index (fndecl, arg_idx);
  inform (DECL_SOURCE_LOCATION (fndecl),
	  "argument %s of %qD must be non-null",
	  arg_desc.get (), fndecl);
  /* Ideally we would use the location of the parm and underline the
     attribute also - but we don't have the location_t values at this point
     in the middle-end.
     For reference, the C and C++ FEs have get_fndecl_argument_location.  */
}

/* Concrete subclass for describing passing a possibly-NULL value to a
   function marked with __attribute__((nonnull)).  */

class possible_null_arg : public possible_null
{
public:
  possible_null_arg (const malloc_state_machine &sm, tree arg,
		     tree fndecl, int arg_idx)
  : possible_null (sm, arg),
    m_fndecl (fndecl), m_arg_idx (arg_idx)
  {}

  const char *get_kind () const final override { return "possible_null_arg"; }

  bool subclass_equal_p (const pending_diagnostic &base_other)
    const final override
  {
    const possible_null_arg &sub_other
      = (const possible_null_arg &)base_other;
    return (same_tree_p (m_arg, sub_other.m_arg)
	    && m_fndecl == sub_other.m_fndecl
	    && m_arg_idx == sub_other.m_arg_idx);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_possible_null_argument;
  }

  bool emit (rich_location *rich_loc) final override
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (690);
    bool warned
      = warning_meta (rich_loc, m, get_controlling_option (),
		      "use of possibly-NULL %qE where non-null expected",
		      m_arg);
    if (warned)
      inform_nonnull_attribute (m_fndecl, m_arg_idx);
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    label_text arg_desc = describe_argument_index (m_fndecl, m_arg_idx);
    label_text result;
    if (m_origin_of_unchecked_event.known_p ())
      result = ev.formatted_print ("argument %s (%qE) from %@ could be NULL"
				   " where non-null expected",
				   arg_desc.get (), ev.m_expr,
				   &m_origin_of_unchecked_event);
    else
      result = ev.formatted_print ("argument %s (%qE) could be NULL"
				   " where non-null expected",
				   arg_desc.get (), ev.m_expr);
    return result;
  }

private:
  tree m_fndecl;
  int m_arg_idx;
};

/* Concrete subclass for describing a dereference of a NULL value.  */

class null_deref : public malloc_diagnostic
{
public:
  null_deref (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg) {}

  const char *get_kind () const final override { return "null_deref"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_null_dereference;
  }

  bool terminate_path_p () const final override { return true; }

  bool emit (rich_location *rich_loc) final override
  {
    /* CWE-476: NULL Pointer Dereference.  */
    diagnostic_metadata m;
    m.add_cwe (476);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "dereference of NULL %qE", m_arg);
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    final override
  {
    if (info.m_state == m_sm.m_null)
      return info.formatted_print ("return of NULL to %qE from %qE",
				   info.m_caller_fndecl, info.m_callee_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    return ev.formatted_print ("dereference of NULL %qE", ev.m_expr);
  }
};

/* Concrete subclass for describing passing a NULL value to a
   function marked with __attribute__((nonnull)).  */

class null_arg : public malloc_diagnostic
{
public:
  null_arg (const malloc_state_machine &sm, tree arg,
	    tree fndecl, int arg_idx)
  : malloc_diagnostic (sm, arg),
    m_fndecl (fndecl), m_arg_idx (arg_idx)
  {}

  const char *get_kind () const final override { return "null_arg"; }

  bool subclass_equal_p (const pending_diagnostic &base_other)
    const final override
  {
    const null_arg &sub_other
      = (const null_arg &)base_other;
    return (same_tree_p (m_arg, sub_other.m_arg)
	    && m_fndecl == sub_other.m_fndecl
	    && m_arg_idx == sub_other.m_arg_idx);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_null_argument;
  }

  bool terminate_path_p () const final override { return true; }

  bool emit (rich_location *rich_loc) final override
  {
    /* CWE-476: NULL Pointer Dereference.  */
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (476);

    bool warned;
    if (zerop (m_arg))
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "use of NULL where non-null expected");
    else
      warned = warning_meta (rich_loc, m, get_controlling_option (),
			     "use of NULL %qE where non-null expected",
			     m_arg);
    if (warned)
      inform_nonnull_attribute (m_fndecl, m_arg_idx);
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    label_text arg_desc = describe_argument_index (m_fndecl, m_arg_idx);
    label_text result;
    if (zerop (ev.m_expr))
      result = ev.formatted_print ("argument %s NULL where non-null expected",
				   arg_desc.get ());
    else
      result = ev.formatted_print ("argument %s (%qE) NULL"
				   " where non-null expected",
				   arg_desc.get (), ev.m_expr);
    return result;
  }

private:
  tree m_fndecl;
  int m_arg_idx;
};

class use_after_free : public malloc_diagnostic
{
public:
  use_after_free (const malloc_state_machine &sm, tree arg,
		  const deallocator *deallocator)
  : malloc_diagnostic (sm, arg),
    m_deallocator (deallocator)
  {
    gcc_assert (deallocator);
  }

  const char *get_kind () const final override { return "use_after_free"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_use_after_free;
  }

  bool emit (rich_location *rich_loc) final override
  {
    /* CWE-416: Use After Free.  */
    diagnostic_metadata m;
    m.add_cwe (416);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "use after %<%s%> of %qE",
			 m_deallocator->m_name, m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (freed_p (change.m_new_state))
      {
	m_free_event = change.m_event_id;
	switch (m_deallocator->m_wording)
	  {
	  default:
	  case WORDING_REALLOCATED:
	    gcc_unreachable ();
	  case WORDING_FREED:
	    return label_text::borrow ("freed here");
	  case WORDING_DELETED:
	    return label_text::borrow ("deleted here");
	  case WORDING_DEALLOCATED:
	    return label_text::borrow ("deallocated here");
	  }
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    const char *funcname = m_deallocator->m_name;
    if (m_free_event.known_p ())
      switch (m_deallocator->m_wording)
	{
	default:
	case WORDING_REALLOCATED:
	  gcc_unreachable ();
	case WORDING_FREED:
	  return ev.formatted_print ("use after %<%s%> of %qE; freed at %@",
				     funcname, ev.m_expr, &m_free_event);
	case WORDING_DELETED:
	  return ev.formatted_print ("use after %<%s%> of %qE; deleted at %@",
				     funcname, ev.m_expr, &m_free_event);
	case WORDING_DEALLOCATED:
	  return ev.formatted_print ("use after %<%s%> of %qE;"
				     " deallocated at %@",
				     funcname, ev.m_expr, &m_free_event);
	}
    else
      return ev.formatted_print ("use after %<%s%> of %qE",
				 funcname, ev.m_expr);
  }

  /* Implementation of pending_diagnostic::supercedes_p for
     use_after_free.

     We want use-after-free to supercede use-of-unitialized-value,
     so that if we have these at the same stmt, we don't emit
     a use-of-uninitialized, just the use-after-free.
     (this is because we fully purge information about freed
     buffers when we free them to avoid state explosions, so
     that if they are accessed after the free, it looks like
     they are uninitialized).  */

  bool supercedes_p (const pending_diagnostic &other) const final override
  {
    if (other.use_of_uninit_p ())
      return true;

    return false;
  }

private:
  diagnostic_event_id_t m_free_event;
  const deallocator *m_deallocator;
};

class malloc_leak : public malloc_diagnostic
{
public:
  malloc_leak (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg) {}

  const char *get_kind () const final override { return "malloc_leak"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_malloc_leak;
  }

  bool emit (rich_location *rich_loc) final override
  {
    /* "CWE-401: Missing Release of Memory after Effective Lifetime".  */
    diagnostic_metadata m;
    m.add_cwe (401);
    if (m_arg)
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "leak of %qE", m_arg);
    else
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "leak of %qs", "<unknown>");
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (unchecked_p (change.m_new_state)
	|| (start_p (change.m_old_state) && nonnull_p (change.m_new_state)))
      {
	m_alloc_event = change.m_event_id;
	return label_text::borrow ("allocated here");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (ev.m_expr)
      {
	if (m_alloc_event.known_p ())
	  return ev.formatted_print ("%qE leaks here; was allocated at %@",
				     ev.m_expr, &m_alloc_event);
	else
	  return ev.formatted_print ("%qE leaks here", ev.m_expr);
      }
    else
      {
	if (m_alloc_event.known_p ())
	  return ev.formatted_print ("%qs leaks here; was allocated at %@",
				     "<unknown>", &m_alloc_event);
	else
	  return ev.formatted_print ("%qs leaks here", "<unknown>");
      }
  }

private:
  diagnostic_event_id_t m_alloc_event;
};

class free_of_non_heap : public malloc_diagnostic
{
public:
  free_of_non_heap (const malloc_state_machine &sm, tree arg,
		    const region *freed_reg,
		    const char *funcname)
  : malloc_diagnostic (sm, arg), m_freed_reg (freed_reg), m_funcname (funcname)
  {
  }

  const char *get_kind () const final override { return "free_of_non_heap"; }

  bool subclass_equal_p (const pending_diagnostic &base_other) const
    final override
  {
    const free_of_non_heap &other = (const free_of_non_heap &)base_other;
    return (same_tree_p (m_arg, other.m_arg)
	    && m_freed_reg == other.m_freed_reg);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_free_of_non_heap;
  }

  bool emit (rich_location *rich_loc) final override
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (590); /* CWE-590: Free of Memory not on the Heap.  */
    switch (get_memory_space ())
      {
      default:
      case MEMSPACE_HEAP:
	gcc_unreachable ();
      case MEMSPACE_UNKNOWN:
      case MEMSPACE_CODE:
      case MEMSPACE_GLOBALS:
      case MEMSPACE_READONLY_DATA:
	return warning_meta (rich_loc, m, get_controlling_option (),
			     "%<%s%> of %qE which points to memory"
			     " not on the heap",
			     m_funcname, m_arg);
	break;
      case MEMSPACE_STACK:
	return warning_meta (rich_loc, m, get_controlling_option (),
			     "%<%s%> of %qE which points to memory"
			     " on the stack",
			     m_funcname, m_arg);
	break;
      }
  }

  label_text describe_state_change (const evdesc::state_change &)
    final override
  {
    return label_text::borrow ("pointer is from here");
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    return ev.formatted_print ("call to %qs here", m_funcname);
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    if (m_freed_reg)
      interest->add_region_creation (m_freed_reg);
  }

private:
  enum memory_space get_memory_space () const
  {
    if (m_freed_reg)
      return m_freed_reg->get_memory_space ();
    else
      return MEMSPACE_UNKNOWN;
  }

  const region *m_freed_reg;
  const char *m_funcname;
};

/* Concrete pending_diagnostic subclass for -Wanalyzer-deref-before-check.  */

class deref_before_check : public malloc_diagnostic
{
public:
  deref_before_check (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg),
    m_deref_enode (NULL),
    m_deref_expr (NULL),
    m_check_enode (NULL)
  {
    gcc_assert (arg);
  }

  const char *get_kind () const final override { return "deref_before_check"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_deref_before_check;
  }

  bool emit (rich_location *rich_loc) final override
  {
    /* Don't emit the warning if we can't show where the deref
       and the check occur.  */
    if (!m_deref_enode)
      return false;
    if (!m_check_enode)
      return false;
    /* Only emit the warning for intraprocedural cases.  */
    const program_point &deref_point = m_deref_enode->get_point ();
    const program_point &check_point = m_check_enode->get_point ();

    if (!program_point::effectively_intraprocedural_p (deref_point,
						       check_point))
      return false;

    /* Reject the warning if the check occurs within a macro defintion.
       This avoids false positives for such code as:

	#define throw_error \
	   do {             \
	     if (p)         \
	       cleanup (p); \
	     return;        \
	   } while (0)

	if (p->idx >= n)
	  throw_error ();

       where the usage of "throw_error" implicitly adds a check
       on 'p'.

       We do warn when the check is in a macro expansion if we can get
       at the location of the condition and it is't part of the
       definition, so that we warn for checks such as:
	   if (words[0][0] == '@')
	     return;
	   g_assert(words[0] != NULL); <--- here
       Unfortunately we don't have locations for individual gimple
       arguments, so in:
	   g_assert (ptr);
       we merely have a gimple_cond
	   if (p_2(D) == 0B)
       with no way of getting at the location of the condition separately
       from that of the gimple_cond (where the "if" is within the macro
       definition).  We reject the warning for such cases.

       We do warn when the *deref* occurs in a macro, since this can be
       a source of real bugs; see e.g. PR 77425.  */
    location_t check_loc = m_check_enode->get_point ().get_location ();
    if (linemap_location_from_macro_definition_p (line_table, check_loc))
      return false;

    /* Reject if m_deref_expr is sufficiently different from m_arg
       for cases where the dereference is spelled differently from
       the check, which is probably two different ways to get the
       same svalue, and thus not worth reporting.  */
    if (!m_deref_expr)
      return false;
    if (!sufficiently_similar_p (m_deref_expr, m_arg))
      return false;

    /* Reject the warning if the deref's BB doesn't dominate that
       of the check, so that we don't warn e.g. for shared cleanup
       code that checks a pointer for NULL, when that code is sometimes
       used before a deref and sometimes after.
       Using the dominance code requires setting cfun.  */
    auto_cfun sentinel (m_deref_enode->get_function ());
    calculate_dominance_info (CDI_DOMINATORS);
    if (!dominated_by_p (CDI_DOMINATORS,
			 m_check_enode->get_supernode ()->m_bb,
			 m_deref_enode->get_supernode ()->m_bb))
      return false;

    return warning_at (rich_loc, get_controlling_option (),
		       "check of %qE for NULL after already"
		       " dereferencing it",
		       m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& assumed_non_null_p (change.m_new_state))
      {
	m_first_deref_event = change.m_event_id;
	m_deref_enode = change.m_event.get_exploded_node ();
	m_deref_expr = change.m_expr;
	return change.formatted_print ("pointer %qE is dereferenced here",
				       m_arg);
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    m_check_enode = ev.m_event.get_exploded_node ();
    if (m_first_deref_event.known_p ())
      return ev.formatted_print ("pointer %qE is checked for NULL here but"
				 " it was already dereferenced at %@",
				 m_arg, &m_first_deref_event);
    else
      return ev.formatted_print ("pointer %qE is checked for NULL here but"
				 " it was already dereferenced",
				 m_arg);
  }

private:
  static bool sufficiently_similar_p (tree expr_a, tree expr_b)
  {
    pretty_printer *pp_a = global_dc->printer->clone ();
    pretty_printer *pp_b = global_dc->printer->clone ();
    pp_printf (pp_a, "%qE", expr_a);
    pp_printf (pp_b, "%qE", expr_b);
    bool result = (strcmp (pp_formatted_text (pp_a), pp_formatted_text (pp_b))
		   == 0);
    delete pp_a;
    delete pp_b;
    return result;
  }

  diagnostic_event_id_t m_first_deref_event;
  const exploded_node *m_deref_enode;
  tree m_deref_expr;
  const exploded_node *m_check_enode;
};

/* struct allocation_state : public state_machine::state.  */

/* Implementation of state_machine::state::dump_to_pp vfunc
   for allocation_state: append the API that this allocation is
   associated with.  */

void
allocation_state::dump_to_pp (pretty_printer *pp) const
{
  state_machine::state::dump_to_pp (pp);
  if (m_deallocators)
    {
      pp_string (pp, " (");
      m_deallocators->dump_to_pp (pp);
      pp_character (pp, ')');
    }
}

/* Given a allocation_state for a deallocator_set, get the "nonnull" state
   for the corresponding allocator(s).  */

const allocation_state *
allocation_state::get_nonnull () const
{
  gcc_assert (m_deallocators);
  return as_a_allocation_state (m_deallocators->m_nonnull);
}

/* struct assumed_non_null_state : public allocation_state.  */

void
assumed_non_null_state::dump_to_pp (pretty_printer *pp) const
{
  allocation_state::dump_to_pp (pp);
  pp_string (pp, " (in ");
  m_frame->dump_to_pp (pp, true);
  pp_character (pp, ')');
}

/* malloc_state_machine's ctor.  */

malloc_state_machine::malloc_state_machine (logger *logger)
: state_machine ("malloc", logger),
  m_free (this, "free", WORDING_FREED),
  m_scalar_delete (this, "delete", WORDING_DELETED),
  m_vector_delete (this, "delete[]", WORDING_DELETED),
  m_realloc (this, "realloc", WORDING_REALLOCATED)
{
  gcc_assert (m_start->get_id () == 0);
  m_null = add_state ("null", RS_FREED, NULL, NULL);
  m_non_heap = add_state ("non-heap", RS_NON_HEAP, NULL, NULL);
  m_stop = add_state ("stop", RS_STOP, NULL, NULL);
}

malloc_state_machine::~malloc_state_machine ()
{
  unsigned i;
  custom_deallocator_set *set;
  FOR_EACH_VEC_ELT (m_dynamic_sets, i, set)
    delete set;
  custom_deallocator *d;
  FOR_EACH_VEC_ELT (m_dynamic_deallocators, i, d)
    delete d;
}

state_machine::state_t
malloc_state_machine::add_state (const char *name, enum resource_state rs,
				 const deallocator_set *deallocators,
				 const deallocator *deallocator)
{
  return add_custom_state (new allocation_state (name, alloc_state_id (),
						 rs, deallocators,
						 deallocator));
}

/* If ALLOCATOR_FNDECL has any "__attribute__((malloc(FOO)))",
   return a custom_deallocator_set for them, consolidating them
   to ensure uniqueness of the sets.

   Return NULL if it has no such attributes.  */

const custom_deallocator_set *
malloc_state_machine::
get_or_create_custom_deallocator_set (tree allocator_fndecl)
{
  /* Early rejection of decls without attributes.  */
  tree attrs = DECL_ATTRIBUTES (allocator_fndecl);
  if (!attrs)
    return NULL;

  /* Otherwise, call maybe_create_custom_deallocator_set,
     memoizing the result.  */
  if (custom_deallocator_set **slot
      = m_custom_deallocator_set_cache.get (allocator_fndecl))
    return *slot;
  custom_deallocator_set *set
    = maybe_create_custom_deallocator_set (allocator_fndecl);
  m_custom_deallocator_set_cache.put (allocator_fndecl, set);
  return set;
}

/* Given ALLOCATOR_FNDECL, a FUNCTION_DECL with attributes,
   look for any "__attribute__((malloc(FOO)))" and return a
   custom_deallocator_set for them, consolidating them
   to ensure uniqueness of the sets.

   Return NULL if it has no such attributes.

   Subroutine of get_or_create_custom_deallocator_set which
   memoizes the result.  */

custom_deallocator_set *
malloc_state_machine::
maybe_create_custom_deallocator_set (tree allocator_fndecl)
{
  tree attrs = DECL_ATTRIBUTES (allocator_fndecl);
  gcc_assert (attrs);

  /* Look for instances of __attribute__((malloc(FOO))).  */
  auto_vec<const deallocator *> deallocator_vec;
  for (tree allocs = attrs;
       (allocs = lookup_attribute ("malloc", allocs));
       allocs = TREE_CHAIN (allocs))
    {
      tree args = TREE_VALUE (allocs);
      if (!args)
	continue;
      if (TREE_VALUE (args))
	{
	  const deallocator *d
	    = get_or_create_deallocator (TREE_VALUE (args));
	  deallocator_vec.safe_push (d);
	}
    }

  /* If there weren't any deallocators, bail.  */
  if (deallocator_vec.length () == 0)
    return NULL;

  /* Consolidate, so that we reuse existing deallocator_set
     instances.  */
  deallocator_vec.qsort (deallocator::cmp_ptr_ptr);
  custom_deallocator_set **slot
    = m_custom_deallocator_set_map.get (&deallocator_vec);
  if (slot)
    return *slot;
  custom_deallocator_set *set
    = new custom_deallocator_set (this, &deallocator_vec, WORDING_DEALLOCATED);
  m_custom_deallocator_set_map.put (&set->m_deallocator_vec, set);
  m_dynamic_sets.safe_push (set);
  return set;
}

/* Get the deallocator for DEALLOCATOR_FNDECL, creating it if necessary.  */

const deallocator *
malloc_state_machine::get_or_create_deallocator (tree deallocator_fndecl)
{
  deallocator **slot = m_deallocator_map.get (deallocator_fndecl);
  if (slot)
    return *slot;

  /* Reuse "free".  */
  deallocator *d;
  if (is_named_call_p (deallocator_fndecl, "free")
      || is_std_named_call_p (deallocator_fndecl, "free")
      || is_named_call_p (deallocator_fndecl, "__builtin_free"))
    d = &m_free.m_deallocator;
  else
    {
      custom_deallocator *cd
	= new custom_deallocator (this, deallocator_fndecl,
				  WORDING_DEALLOCATED);
      m_dynamic_deallocators.safe_push (cd);
      d = cd;
    }
  m_deallocator_map.put (deallocator_fndecl, d);
  return d;
}

/* Get the "assumed-non-null" state for assumptions made within FRAME,
   creating it if necessary.  */

state_machine::state_t
malloc_state_machine::
get_or_create_assumed_non_null_state_for_frame (const frame_region *frame)
{
  if (state_t *slot = m_assumed_non_null.get (frame))
    return *slot;
  state_machine::state *new_state
    = new assumed_non_null_state ("assumed-non-null", alloc_state_id (), frame);
  add_custom_state (new_state);
  m_assumed_non_null.put (frame, new_state);
  return new_state;
}

/* Try to identify the function declaration either by name or as a known malloc
   builtin.  */

static bool
known_allocator_p (const_tree fndecl, const gcall *call)
{
  /* Either it is a function we know by name and number of arguments... */
  if (is_named_call_p (fndecl, "malloc", call, 1)
      || is_named_call_p (fndecl, "calloc", call, 2)
      || is_std_named_call_p (fndecl, "malloc", call, 1)
      || is_std_named_call_p (fndecl, "calloc", call, 2)
      || is_named_call_p (fndecl, "strdup", call, 1)
      || is_named_call_p (fndecl, "strndup", call, 2))
    return true;

  /* ... or it is a builtin allocator that allocates objects freed with
     __builtin_free.  */
  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      case BUILT_IN_MALLOC:
      case BUILT_IN_CALLOC:
      case BUILT_IN_STRDUP:
      case BUILT_IN_STRNDUP:
	return true;
      default:
	break;
      }

  return false;
}

/* If PTR's nullness is not known, transition it to the "assumed-non-null"
   state for the current frame.  */

void
malloc_state_machine::maybe_assume_non_null (sm_context *sm_ctxt,
					     tree ptr,
					     const gimple *stmt) const
{
  const region_model *old_model = sm_ctxt->get_old_region_model ();
  if (!old_model)
    return;

  tree null_ptr_cst = build_int_cst (TREE_TYPE (ptr), 0);
  tristate known_non_null
    = old_model->eval_condition (ptr, NE_EXPR, null_ptr_cst, NULL);
  if (known_non_null.is_unknown ())
    {
      /* Cast away const-ness for cache-like operations.  */
      malloc_state_machine *mut_this
	= const_cast <malloc_state_machine *> (this);
      state_t next_state
	= mut_this->get_or_create_assumed_non_null_state_for_frame
	(old_model->get_current_frame ());
      sm_ctxt->set_next_state (stmt, ptr, next_state);
    }
}

/* Implementation of state_machine::on_stmt vfunc for malloc_state_machine.  */

bool
malloc_state_machine::on_stmt (sm_context *sm_ctxt,
			       const supernode *node,
			       const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (known_allocator_p (callee_fndecl, call))
	  {
	    on_allocator_call (sm_ctxt, call, &m_free);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "operator new", call, 1))
	  on_allocator_call (sm_ctxt, call, &m_scalar_delete);
	else if (is_named_call_p (callee_fndecl, "operator new []", call, 1))
	  on_allocator_call (sm_ctxt, call, &m_vector_delete);
	else if (is_named_call_p (callee_fndecl, "operator delete", call, 1)
		 || is_named_call_p (callee_fndecl, "operator delete", call, 2))
	  {
	    on_deallocator_call (sm_ctxt, node, call,
				 &m_scalar_delete.m_deallocator, 0);
	    return true;
	  }
	else if (is_named_call_p (callee_fndecl, "operator delete []", call, 1))
	  {
	    on_deallocator_call (sm_ctxt, node, call,
				 &m_vector_delete.m_deallocator, 0);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "alloca", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_alloca", call, 1))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      sm_ctxt->on_transition (node, stmt, lhs, m_start, m_non_heap);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "free", call, 1)
	    || is_std_named_call_p (callee_fndecl, "free", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_free", call, 1))
	  {
	    on_deallocator_call (sm_ctxt, node, call,
				 &m_free.m_deallocator, 0);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "realloc", call, 2)
	    || is_named_call_p (callee_fndecl, "__builtin_realloc", call, 2))
	  {
	    on_realloc_call (sm_ctxt, node, call);
	    return true;
	  }

	if (unaffected_by_call_p (callee_fndecl))
	  return true;

	/* Cast away const-ness for cache-like operations.  */
	malloc_state_machine *mutable_this
	  = const_cast <malloc_state_machine *> (this);

	/* Handle "__attribute__((malloc(FOO)))".   */
	if (const deallocator_set *deallocators
	      = mutable_this->get_or_create_custom_deallocator_set
		  (callee_fndecl))
	  {
	    tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (callee_fndecl));
	    bool returns_nonnull
	      = lookup_attribute ("returns_nonnull", attrs);
	    on_allocator_call (sm_ctxt, call, deallocators, returns_nonnull);
	  }

	/* Handle "__attribute__((nonnull))".   */
	{
	  tree fntype = TREE_TYPE (callee_fndecl);
	  bitmap nonnull_args = get_nonnull_args (fntype);
	  if (nonnull_args)
	    {
	      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  tree arg = gimple_call_arg (stmt, i);
		  if (TREE_CODE (TREE_TYPE (arg)) != POINTER_TYPE)
		    continue;
		  /* If we have a nonnull-args, and either all pointers, or just
		     the specified pointers.  */
		  if (bitmap_empty_p (nonnull_args)
		      || bitmap_bit_p (nonnull_args, i))
		    {
		      state_t state = sm_ctxt->get_state (stmt, arg);
		      /* Can't use a switch as the states are non-const.  */
		      if (unchecked_p (state))
			{
			  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
			  sm_ctxt->warn (node, stmt, arg,
					 make_unique<possible_null_arg>
					   (*this, diag_arg, callee_fndecl, i));
			  const allocation_state *astate
			    = as_a_allocation_state (state);
			  sm_ctxt->set_next_state (stmt, arg,
						   astate->get_nonnull ());
			}
		      else if (state == m_null)
			{
			  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
			  sm_ctxt->warn (node, stmt, arg,
					 make_unique<null_arg>
					   (*this, diag_arg, callee_fndecl, i));
			  sm_ctxt->set_next_state (stmt, arg, m_stop);
			}
		      else if (state == m_start)
			maybe_assume_non_null (sm_ctxt, arg, stmt);
		    }
		}
	      BITMAP_FREE (nonnull_args);
	    }
	}

	/* Check for this after nonnull, so that if we have both
	   then we transition to "freed", rather than "checked".  */
	unsigned dealloc_argno = fndecl_dealloc_argno (callee_fndecl);
	if (dealloc_argno != UINT_MAX)
	  {
	    const deallocator *d
	      = mutable_this->get_or_create_deallocator (callee_fndecl);
	    on_deallocator_call (sm_ctxt, node, call, d, dealloc_argno);
	  }
      }

  /* Look for pointers explicitly being compared against zero
     that are in state assumed_non_null i.e. we already defererenced
     them.
     We have to do this check here, rather than in on_condition
     because we add a constraint that the pointer is non-null when
     dereferencing it, and this makes the apply_constraints_for_gcond
     find known-true and known-false conditions; on_condition is only
     called when adding new constraints.  */
  if (const gcond *cond_stmt = dyn_cast <const gcond *> (stmt))
    {
      enum tree_code op = gimple_cond_code (cond_stmt);
      if (op == EQ_EXPR || op == NE_EXPR)
	{
	  tree lhs = gimple_cond_lhs (cond_stmt);
	  tree rhs = gimple_cond_rhs (cond_stmt);
	  if (any_pointer_p (lhs)
	      && any_pointer_p (rhs)
	      && zerop (rhs))
	    {
	      state_t state = sm_ctxt->get_state (stmt, lhs);
	      if (assumed_non_null_p (state))
		maybe_complain_about_deref_before_check
		  (sm_ctxt, node,
		   stmt,
		   (const assumed_non_null_state *)state,
		   lhs);
	    }
	}
    }

  if (tree lhs = sm_ctxt->is_zero_assignment (stmt))
    if (any_pointer_p (lhs))
      on_zero_assignment (sm_ctxt, stmt,lhs);

  /* Handle dereferences.  */
  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
    {
      tree op = gimple_op (stmt, i);
      if (!op)
	continue;
      if (TREE_CODE (op) == COMPONENT_REF)
	op = TREE_OPERAND (op, 0);

      if (TREE_CODE (op) == MEM_REF)
	{
	  tree arg = TREE_OPERAND (op, 0);

	  state_t state = sm_ctxt->get_state (stmt, arg);
	  if (state == m_start)
	    maybe_assume_non_null (sm_ctxt, arg, stmt);
	  else if (unchecked_p (state))
	    {
	      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	      sm_ctxt->warn (node, stmt, arg,
			     make_unique<possible_null_deref> (*this,
							       diag_arg));
	      const allocation_state *astate = as_a_allocation_state (state);
	      sm_ctxt->set_next_state (stmt, arg, astate->get_nonnull ());
	    }
	  else if (state == m_null)
	    {
	      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	      sm_ctxt->warn (node, stmt, arg,
			     make_unique<null_deref> (*this, diag_arg));
	      sm_ctxt->set_next_state (stmt, arg, m_stop);
	    }
	  else if (freed_p (state))
	    {
	      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	      const allocation_state *astate = as_a_allocation_state (state);
	      sm_ctxt->warn (node, stmt, arg,
			     make_unique<use_after_free>
			       (*this, diag_arg, astate->m_deallocator));
	      sm_ctxt->set_next_state (stmt, arg, m_stop);
	    }
	}
    }
  return false;
}

/* Given a check against null of PTR in assumed-non-null state STATE,
   potentially add a deref_before_check warning to SM_CTXT.  */

void
malloc_state_machine::
maybe_complain_about_deref_before_check (sm_context *sm_ctxt,
					 const supernode *node,
					 const gimple *stmt,
					 const assumed_non_null_state *state,
					 tree ptr) const
{
  const region_model *model = sm_ctxt->get_old_region_model ();
  if (!model)
    return;

  /* Don't complain if the current frame (where the check is occurring) is
     deeper than the frame in which the "not null" assumption was made.
     This suppress false positives for cases like:

	void foo (struct s *p)
	{
	  int val = s->some_field; // deref here
	  shared_helper (p);
	}

     where "shared_helper" has:

	void shared_helper (struct s *p)
	{
	  if (!p) // check here
	    return;
	  // etc
	}

     since the check in "shared_helper" is OK.  */
  const frame_region *checked_in_frame = model->get_current_frame ();
  const frame_region *assumed_nonnull_in_frame = state->m_frame;
  if (checked_in_frame->get_index () > assumed_nonnull_in_frame->get_index ())
    return;

  tree diag_ptr = sm_ctxt->get_diagnostic_tree (ptr);
  if (diag_ptr)
    sm_ctxt->warn
      (node, stmt, ptr,
       make_unique<deref_before_check> (*this, diag_ptr));
  sm_ctxt->set_next_state (stmt, ptr, m_stop);
}

/* Handle a call to an allocator.
   RETURNS_NONNULL is true if CALL is to a fndecl known to have
   __attribute__((returns_nonnull)).  */

void
malloc_state_machine::on_allocator_call (sm_context *sm_ctxt,
					 const gcall *call,
					 const deallocator_set *deallocators,
					 bool returns_nonnull) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    {
      if (sm_ctxt->get_state (call, lhs) == m_start)
	sm_ctxt->set_next_state (call, lhs,
				 (returns_nonnull
				  ? deallocators->m_nonnull
				  : deallocators->m_unchecked));
    }
  else
    {
      /* TODO: report leak.  */
    }
}

/* Handle deallocations of non-heap pointers.
   non-heap -> stop, with warning.  */

void
malloc_state_machine::handle_free_of_non_heap (sm_context *sm_ctxt,
					       const supernode *node,
					       const gcall *call,
					       tree arg,
					       const deallocator *d) const
{
  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
  const region *freed_reg = NULL;
  if (const program_state *old_state = sm_ctxt->get_old_program_state ())
    {
      const region_model *old_model = old_state->m_region_model;
      const svalue *ptr_sval = old_model->get_rvalue (arg, NULL);
      freed_reg = old_model->deref_rvalue (ptr_sval, arg, NULL);
    }
  sm_ctxt->warn (node, call, arg,
		 make_unique<free_of_non_heap>
		   (*this, diag_arg, freed_reg, d->m_name));
  sm_ctxt->set_next_state (call, arg, m_stop);
}

void
malloc_state_machine::on_deallocator_call (sm_context *sm_ctxt,
					   const supernode *node,
					   const gcall *call,
					   const deallocator *d,
					   unsigned argno) const
{
  if (argno >= gimple_call_num_args (call))
    return;
  tree arg = gimple_call_arg (call, argno);

  state_t state = sm_ctxt->get_state (call, arg);

  /* start/assumed_non_null/unchecked/nonnull -> freed.  */
  if (state == m_start || assumed_non_null_p (state))
    sm_ctxt->set_next_state (call, arg, d->m_freed);
  else if (unchecked_p (state) || nonnull_p (state))
    {
      const allocation_state *astate = as_a_allocation_state (state);
      gcc_assert (astate->m_deallocators);
      if (!astate->m_deallocators->contains_p (d))
	{
	  /* Wrong allocator.  */
	  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	  sm_ctxt->warn (node, call, arg,
			 make_unique<mismatching_deallocation>
			   (*this, diag_arg,
			    astate->m_deallocators,
			    d));
	}
      sm_ctxt->set_next_state (call, arg, d->m_freed);
    }

  /* Keep state "null" as-is, rather than transitioning to "freed";
     we don't want to complain about double-free of NULL.  */
  else if (state == d->m_freed)
    {
      /* freed -> stop, with warning.  */
      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
      sm_ctxt->warn (node, call, arg,
		     make_unique<double_free> (*this, diag_arg, d->m_name));
      sm_ctxt->set_next_state (call, arg, m_stop);
    }
  else if (state == m_non_heap)
    {
      /* non-heap -> stop, with warning.  */
      handle_free_of_non_heap (sm_ctxt, node, call, arg, d);
    }
}

/* Handle a call to "realloc".
   Check for free of non-heap or mismatching allocators,
   transitioning to the "stop" state for such cases.

   Otherwise, kf_realloc::impl_call_post will later
   get called (which will handle other sm-state transitions
   when the state is bifurcated).  */

void
malloc_state_machine::on_realloc_call (sm_context *sm_ctxt,
				       const supernode *node,
				       const gcall *call) const
{
  const unsigned argno = 0;
  const deallocator *d = &m_realloc;

  tree arg = gimple_call_arg (call, argno);

  state_t state = sm_ctxt->get_state (call, arg);

  if (unchecked_p (state) || nonnull_p (state))
    {
      const allocation_state *astate = as_a_allocation_state (state);
      gcc_assert (astate->m_deallocators);
      if (!astate->m_deallocators->contains_p (&m_free.m_deallocator))
	{
	  /* Wrong allocator.  */
	  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	  sm_ctxt->warn (node, call, arg,
			 make_unique<mismatching_deallocation>
			   (*this, diag_arg,
			    astate->m_deallocators, d));
	  sm_ctxt->set_next_state (call, arg, m_stop);
	  if (path_context *path_ctxt = sm_ctxt->get_path_context ())
	    path_ctxt->terminate_path ();
	}
    }
  else if (state == m_free.m_deallocator.m_freed)
    {
      /* freed -> stop, with warning.  */
      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
      sm_ctxt->warn (node, call, arg,
		     make_unique<double_free> (*this, diag_arg, "free"));
      sm_ctxt->set_next_state (call, arg, m_stop);
      if (path_context *path_ctxt = sm_ctxt->get_path_context ())
	path_ctxt->terminate_path ();
    }
  else if (state == m_non_heap)
    {
      /* non-heap -> stop, with warning.  */
      handle_free_of_non_heap (sm_ctxt, node, call, arg, d);
      if (path_context *path_ctxt = sm_ctxt->get_path_context ())
	path_ctxt->terminate_path ();
    }
}

/* Implementation of state_machine::on_phi vfunc for malloc_state_machine.  */

void
malloc_state_machine::on_phi (sm_context *sm_ctxt,
			      const supernode *node ATTRIBUTE_UNUSED,
			      const gphi *phi,
			      tree rhs) const
{
  if (zerop (rhs))
    {
      tree lhs = gimple_phi_result (phi);
      on_zero_assignment (sm_ctxt, phi, lhs);
    }
}

/* Implementation of state_machine::on_condition vfunc for malloc_state_machine.
   Potentially transition state 'unchecked' to 'nonnull' or to 'null'.  */

void
malloc_state_machine::on_condition (sm_context *sm_ctxt,
				    const supernode *node ATTRIBUTE_UNUSED,
				    const gimple *stmt,
				    const svalue *lhs,
				    enum tree_code op,
				    const svalue *rhs) const
{
  if (!rhs->all_zeroes_p ())
    return;

  if (!any_pointer_p (lhs))
    return;
  if (!any_pointer_p (rhs))
    return;

  if (op == NE_EXPR)
    {
      log ("got 'ARG != 0' match");
      state_t s = sm_ctxt->get_state (stmt, lhs);
      if (unchecked_p (s))
	{
	  const allocation_state *astate = as_a_allocation_state (s);
	  sm_ctxt->set_next_state (stmt, lhs, astate->get_nonnull ());
	}
    }
  else if (op == EQ_EXPR)
    {
      log ("got 'ARG == 0' match");
      state_t s = sm_ctxt->get_state (stmt, lhs);
      if (unchecked_p (s))
	sm_ctxt->set_next_state (stmt, lhs, m_null);
    }
}

/* Implementation of state_machine::on_pop_frame vfunc for malloc_state_machine.
   Clear any "assumed-non-null" state where the assumption happened in
   FRAME_REG.  */

void
malloc_state_machine::on_pop_frame (sm_state_map *smap,
				    const frame_region *frame_reg) const
{
  hash_set<const svalue *> svals_to_clear;
  for (auto kv : *smap)
    {
      const svalue *sval = kv.first;
      state_t state = kv.second.m_state;
      if (assumed_non_null_p (state))
	{
	  const assumed_non_null_state *assumed_state
	    = (const assumed_non_null_state *)state;
	  if (frame_reg == assumed_state->m_frame)
	    svals_to_clear.add (sval);
	}
    }
  for (auto sval : svals_to_clear)
    smap->clear_any_state (sval);
}

/* Implementation of state_machine::can_purge_p vfunc for malloc_state_machine.
   Don't allow purging of pointers in state 'unchecked' or 'nonnull'
   (to avoid false leak reports).  */

bool
malloc_state_machine::can_purge_p (state_t s) const
{
  enum resource_state rs = get_rs (s);
  return rs != RS_UNCHECKED && rs != RS_NONNULL;
}

/* Implementation of state_machine::on_leak vfunc for malloc_state_machine
   (for complaining about leaks of pointers in state 'unchecked' and
   'nonnull').  */

std::unique_ptr<pending_diagnostic>
malloc_state_machine::on_leak (tree var) const
{
  return make_unique<malloc_leak> (*this, var);
}

/* Implementation of state_machine::reset_when_passed_to_unknown_fn_p vfunc
   for malloc_state_machine.  */

bool
malloc_state_machine::reset_when_passed_to_unknown_fn_p (state_t s,
							 bool is_mutable) const
{
  /* An on-stack ptr doesn't stop being stack-allocated when passed to an
     unknown fn.  */
  if (s == m_non_heap)
    return false;

  /* Otherwise, pointers passed as non-const can be freed.  */
  return is_mutable;
}

/* Implementation of state_machine::maybe_get_merged_states_nonequal vfunc
   for malloc_state_machine.

   Support discarding "assumed-non-null" states when merging with
   start state.  */

state_machine::state_t
malloc_state_machine::maybe_get_merged_states_nonequal (state_t state_a,
							state_t state_b) const
{
  if (assumed_non_null_p (state_a) && state_b == m_start)
    return m_start;
  if (state_a == m_start && assumed_non_null_p (state_b))
    return m_start;
  return NULL;
}

/* Return true if calls to FNDECL are known to not affect this sm-state.  */

bool
malloc_state_machine::unaffected_by_call_p (tree fndecl)
{
  /* A set of functions that are known to not affect allocation
     status, even if we haven't fully modelled the rest of their
     behavior yet.  */
  static const char * const funcnames[] = {
    /* This array must be kept sorted.  */
    "strsep",
  };
  const size_t count = ARRAY_SIZE (funcnames);
  function_set fs (funcnames, count);

  if (fs.contains_decl_p (fndecl))
    return true;

  return false;
}

/* Shared logic for handling GIMPLE_ASSIGNs and GIMPLE_PHIs that
   assign zero to LHS.  */

void
malloc_state_machine::on_zero_assignment (sm_context *sm_ctxt,
					  const gimple *stmt,
					  tree lhs) const
{
  state_t s = sm_ctxt->get_state (stmt, lhs);
  enum resource_state rs = get_rs (s);
  if (rs == RS_START
      || rs == RS_UNCHECKED
      || rs == RS_NONNULL
      || rs == RS_FREED)
    sm_ctxt->set_next_state (stmt, lhs, m_null);
}

/* Special-case hook for handling realloc, for the "success with move to
   a new buffer" case, marking OLD_PTR_SVAL as freed and NEW_PTR_SVAL as
   non-null.

   This is similar to on_deallocator_call and on_allocator_call,
   but the checks happen in on_realloc_call, and by splitting the states.  */

void
malloc_state_machine::
on_realloc_with_move (region_model *model,
		      sm_state_map *smap,
		      const svalue *old_ptr_sval,
		      const svalue *new_ptr_sval,
		      const extrinsic_state &ext_state) const
{
  smap->set_state (model, old_ptr_sval,
		   m_free.m_deallocator.m_freed,
		   NULL, ext_state);

  smap->set_state (model, new_ptr_sval,
		   m_free.m_nonnull,
		   NULL, ext_state);
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_malloc_state_machine (logger *logger)
{
  return new malloc_state_machine (logger);
}

/* Specialcase hook for handling realloc, for use by
   kf_realloc::impl_call_post::success_with_move::update_model.  */

void
region_model::on_realloc_with_move (const call_details &cd,
				    const svalue *old_ptr_sval,
				    const svalue *new_ptr_sval)
{
  region_model_context *ctxt = cd.get_ctxt ();
  if (!ctxt)
    return;
  const extrinsic_state *ext_state = ctxt->get_ext_state ();
  if (!ext_state)
    return;

  sm_state_map *smap;
  const state_machine *sm;
  unsigned sm_idx;
  if (!ctxt->get_malloc_map (&smap, &sm, &sm_idx))
    return;

  gcc_assert (smap);
  gcc_assert (sm);

  const malloc_state_machine &malloc_sm
    = (const malloc_state_machine &)*sm;

  malloc_sm.on_realloc_with_move (this,
				  smap,
				  old_ptr_sval,
				  new_ptr_sval,
				  *ext_state);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
