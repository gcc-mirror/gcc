/* Subclasses of diagnostic_event for analyzer diagnostics.
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

#ifndef GCC_ANALYZER_CHECKER_EVENT_H
#define GCC_ANALYZER_CHECKER_EVENT_H

#include "tree-logical-location.h"
#include "analyzer/program-state.h"

namespace ana {

/* A bundle of location information for a checker_event.  */

struct event_loc_info
{
  event_loc_info (location_t loc, tree fndecl, int depth)
  : m_loc (loc), m_fndecl (fndecl), m_depth (depth)
  {}

  location_t m_loc;
  tree m_fndecl;
  int m_depth;
};

/* An enum for discriminating between the concrete subclasses of
   checker_event.  */

enum event_kind
{
  EK_DEBUG,
  EK_CUSTOM,
  EK_STMT,
  EK_REGION_CREATION,
  EK_FUNCTION_ENTRY,
  EK_STATE_CHANGE,
  EK_START_CFG_EDGE,
  EK_END_CFG_EDGE,
  EK_CALL_EDGE,
  EK_RETURN_EDGE,
  EK_START_CONSOLIDATED_CFG_EDGES,
  EK_END_CONSOLIDATED_CFG_EDGES,
  EK_INLINED_CALL,
  EK_SETJMP,
  EK_REWIND_FROM_LONGJMP,
  EK_REWIND_TO_SETJMP,
  EK_WARNING
};

extern const char *event_kind_to_string (enum event_kind ek);

/* Event subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with event_kinds shown for the concrete subclasses):

   diagnostic_event
     checker_event
       debug_event (EK_DEBUG)
       custom_event (EK_CUSTOM)
	 precanned_custom_event
       statement_event (EK_STMT)
       region_creation_event (EK_REGION_CREATION)
       function_entry_event (EK_FUNCTION_ENTRY)
       state_change_event (EK_STATE_CHANGE)
       superedge_event
         cfg_edge_event
	   start_cfg_edge_event (EK_START_CFG_EDGE)
	   end_cfg_edge_event (EK_END_CFG_EDGE)
         call_event (EK_CALL_EDGE)
         return_edge (EK_RETURN_EDGE)
       start_consolidated_cfg_edges_event (EK_START_CONSOLIDATED_CFG_EDGES)
       end_consolidated_cfg_edges_event (EK_END_CONSOLIDATED_CFG_EDGES)
       inlined_call_event (EK_INLINED_CALL)
       setjmp_event (EK_SETJMP)
       rewind_event
         rewind_from_longjmp_event (EK_REWIND_FROM_LONGJMP)
	 rewind_to_setjmp_event (EK_REWIND_TO_SETJMP)
       warning_event (EK_WARNING).  */

/* Abstract subclass of diagnostic_event; the base class for use in
   checker_path (the analyzer's diagnostic_path subclass).  */

class checker_event : public diagnostic_event
{
public:
  /* Implementation of diagnostic_event.  */

  location_t get_location () const final override { return m_loc; }
  tree get_fndecl () const final override { return m_effective_fndecl; }
  int get_stack_depth () const final override { return m_effective_depth; }
  const logical_location *get_logical_location () const final override
  {
    if (m_effective_fndecl)
      return &m_logical_loc;
    else
      return NULL;
  }
  meaning get_meaning () const override;
  diagnostic_thread_id_t get_thread_id () const final override
  {
    return 0;
  }

  /* Additional functionality.  */

  int get_original_stack_depth () const { return m_original_depth; }

  virtual void prepare_for_emission (checker_path *,
				     pending_diagnostic *pd,
				     diagnostic_event_id_t emission_id);
  virtual bool is_call_p () const { return false; }
  virtual bool is_function_entry_p () const  { return false; }
  virtual bool is_return_p () const  { return false; }

  /* For use with %@.  */
  const diagnostic_event_id_t *get_id_ptr () const
  {
    return &m_emission_id;
  }

  void dump (pretty_printer *pp) const;
  void debug () const;

  void set_location (location_t loc) { m_loc = loc; }

protected:
  checker_event (enum event_kind kind,
		 const event_loc_info &loc_info);

 public:
  const enum event_kind m_kind;
 protected:
  location_t m_loc;
  tree m_original_fndecl;
  tree m_effective_fndecl;
  int m_original_depth;
  int m_effective_depth;
  pending_diagnostic *m_pending_diagnostic;
  diagnostic_event_id_t m_emission_id; // only set once all pruning has occurred
  tree_logical_location m_logical_loc;
};

/* A concrete event subclass for a purely textual event, for use in
   debugging path creation and filtering.  */

class debug_event : public checker_event
{
public:

  debug_event (const event_loc_info &loc_info,
	       const char *desc)
  : checker_event (EK_DEBUG, loc_info),
    m_desc (xstrdup (desc))
  {
  }
  ~debug_event ()
  {
    free (m_desc);
  }

  label_text get_desc (bool) const final override;

private:
  char *m_desc;
};

/* An abstract event subclass for custom events.  These are not filtered,
   as they are likely to be pertinent to the diagnostic.  */

class custom_event : public checker_event
{
protected:
  custom_event (const event_loc_info &loc_info)
  : checker_event (EK_CUSTOM, loc_info)
  {
  }
};

/* A concrete custom_event subclass with a precanned message.  */

class precanned_custom_event : public custom_event
{
public:
  precanned_custom_event (const event_loc_info &loc_info,
			  const char *desc)
  : custom_event (loc_info),
    m_desc (xstrdup (desc))
  {
  }
  ~precanned_custom_event ()
  {
    free (m_desc);
  }

  label_text get_desc (bool) const final override;

private:
  char *m_desc;
};

/* A concrete event subclass describing the execution of a gimple statement,
   for use at high verbosity levels when debugging paths.  */

class statement_event : public checker_event
{
public:
  statement_event (const gimple *stmt, tree fndecl, int depth,
		   const program_state &dst_state);

  label_text get_desc (bool) const final override;

  const gimple * const m_stmt;
  const program_state m_dst_state;
};

/* An abstract event subclass describing the creation of a region that
   is significant for a diagnostic.

   There are too many combinations to express region creation in one message,
   so we emit multiple region_creation_event instances when each pertinent
   region is created.

   The events are created by pending_diagnostic's add_region_creation_events
   vfunc, which by default creates a region_creation_event_memory_space, and
   if a capacity is known, a region_creation_event_capacity, giving e.g.:
     (1) region created on stack here
     (2) capacity: 100 bytes
   but this vfunc can be overridden to create other events if other wordings
   are more appropriate foa a given pending_diagnostic.  */

class region_creation_event : public checker_event
{
protected:
  region_creation_event (const event_loc_info &loc_info);
};

/* Concrete subclass of region_creation_event.
   Generates a message based on the memory space of the region
   e.g. "region created on stack here".  */

class region_creation_event_memory_space : public region_creation_event
{
public:
  region_creation_event_memory_space (enum memory_space mem_space,
				      const event_loc_info &loc_info)
  : region_creation_event (loc_info),
    m_mem_space (mem_space)
  {
  }

  label_text get_desc (bool can_colorize) const final override;

private:
  enum memory_space m_mem_space;
};

/* Concrete subclass of region_creation_event.
   Generates a message based on the capacity of the region
   e.g. "capacity: 100 bytes".  */

class region_creation_event_capacity : public region_creation_event
{
public:
  region_creation_event_capacity (tree capacity,
				  const event_loc_info &loc_info)
  : region_creation_event (loc_info),
    m_capacity (capacity)
  {
    gcc_assert (m_capacity);
  }

  label_text get_desc (bool can_colorize) const final override;

private:
  tree m_capacity;
};

/* Concrete subclass of region_creation_event.
   Generates a message based on the capacity of the region
   e.g. "allocated 100 bytes here".  */

class region_creation_event_allocation_size : public region_creation_event
{
public:
  region_creation_event_allocation_size (tree capacity,
					 const event_loc_info &loc_info)
  : region_creation_event (loc_info),
    m_capacity (capacity)
  {}

  label_text get_desc (bool can_colorize) const final override;

private:
  tree m_capacity;
};

/* Concrete subclass of region_creation_event.
   Generates a debug message intended for analyzer developers.  */

class region_creation_event_debug : public region_creation_event
{
public:
  region_creation_event_debug (const region *reg, tree capacity,
			       const event_loc_info &loc_info)
  : region_creation_event (loc_info),
    m_reg (reg), m_capacity (capacity)
  {
  }

  label_text get_desc (bool can_colorize) const final override;

private:
  const region *m_reg;
  tree m_capacity;
};

/* An event subclass describing the entry to a function.  */

class function_entry_event : public checker_event
{
public:
  function_entry_event (const event_loc_info &loc_info)
  : checker_event (EK_FUNCTION_ENTRY, loc_info)
  {
  }

  function_entry_event (const program_point &dst_point);

  label_text get_desc (bool can_colorize) const override;
  meaning get_meaning () const override;

  bool is_function_entry_p () const final override { return true; }
};

/* Subclass of checker_event describing a state change.  */

class state_change_event : public checker_event
{
public:
  state_change_event (const supernode *node, const gimple *stmt,
		      int stack_depth,
		      const state_machine &sm,
		      const svalue *sval,
		      state_machine::state_t from,
		      state_machine::state_t to,
		      const svalue *origin,
		      const program_state &dst_state,
		      const exploded_node *enode);

  label_text get_desc (bool can_colorize) const final override;
  meaning get_meaning () const override;

  function *get_dest_function () const
  {
    return m_dst_state.get_current_function ();
  }

  const exploded_node *get_exploded_node () const { return m_enode; }

  const supernode *m_node;
  const gimple *m_stmt;
  const state_machine &m_sm;
  const svalue *m_sval;
  state_machine::state_t m_from;
  state_machine::state_t m_to;
  const svalue *m_origin;
  program_state m_dst_state;
  const exploded_node *m_enode;
};

/* Subclass of checker_event; parent class for subclasses that relate to
   a superedge.  */

class superedge_event : public checker_event
{
public:
  /* Mark this edge event as being either an interprocedural call or
     return in which VAR is in STATE, and that this is critical to the
     diagnostic (so that get_desc can attempt to get a better description
     from any pending_diagnostic).  */
  void record_critical_state (tree var, state_machine::state_t state)
  {
    m_var = var;
    m_critical_state = state;
  }

  const callgraph_superedge& get_callgraph_superedge () const;

  bool should_filter_p (int verbosity) const;

 protected:
  superedge_event (enum event_kind kind, const exploded_edge &eedge,
		   const event_loc_info &loc_info);

 public:
  const exploded_edge &m_eedge;
  const superedge *m_sedge;
  tree m_var;
  state_machine::state_t m_critical_state;
};

/* An abstract event subclass for when a CFG edge is followed; it has two
   subclasses, representing the start of the edge and the end of the
   edge, which come in pairs.  */

class cfg_edge_event : public superedge_event
{
public:
  meaning get_meaning () const override;

  const cfg_superedge& get_cfg_superedge () const;

 protected:
  cfg_edge_event (enum event_kind kind, const exploded_edge &eedge,
		  const event_loc_info &loc_info);
};

/* A concrete event subclass for the start of a CFG edge
   e.g. "following 'false' branch...'.  */

class start_cfg_edge_event : public cfg_edge_event
{
public:
  start_cfg_edge_event (const exploded_edge &eedge,
			const event_loc_info &loc_info)
  : cfg_edge_event (EK_START_CFG_EDGE, eedge, loc_info)
  {
  }

  label_text get_desc (bool can_colorize) const override;

protected:
  label_text maybe_describe_condition (bool can_colorize) const;

private:
  static label_text maybe_describe_condition (bool can_colorize,
					      tree lhs,
					      enum tree_code op,
					      tree rhs);
  static bool should_print_expr_p (tree);
};

/* A concrete event subclass for the end of a CFG edge
   e.g. "...to here'.  */

class end_cfg_edge_event : public cfg_edge_event
{
public:
  end_cfg_edge_event (const exploded_edge &eedge,
		      const event_loc_info &loc_info)
  : cfg_edge_event (EK_END_CFG_EDGE, eedge, loc_info)
  {
  }

  label_text get_desc (bool /*can_colorize*/) const final override
  {
    return label_text::borrow ("...to here");
  }
};

/* A concrete event subclass for an interprocedural call.  */

class call_event : public superedge_event
{
public:
  call_event (const exploded_edge &eedge,
	      const event_loc_info &loc_info);

  label_text get_desc (bool can_colorize) const override;
  meaning get_meaning () const override;

  bool is_call_p () const final override;

protected:
  tree get_caller_fndecl () const;
  tree get_callee_fndecl () const;

  const supernode *m_src_snode;
  const supernode *m_dest_snode;
};

/* A concrete event subclass for an interprocedural return.  */

class return_event : public superedge_event
{
public:
  return_event (const exploded_edge &eedge,
		const event_loc_info &loc_info);

  label_text get_desc (bool can_colorize) const final override;
  meaning get_meaning () const override;

  bool is_return_p () const final override;

  const supernode *m_src_snode;
  const supernode *m_dest_snode;
};

/* A concrete event subclass for the start of a consolidated run of CFG
   edges all either TRUE or FALSE e.g. "following 'false' branch...'.  */

class start_consolidated_cfg_edges_event : public checker_event
{
public:
  start_consolidated_cfg_edges_event (const event_loc_info &loc_info,
				      bool edge_sense)
  : checker_event (EK_START_CONSOLIDATED_CFG_EDGES, loc_info),
    m_edge_sense (edge_sense)
  {
  }

  label_text get_desc (bool can_colorize) const final override;
  meaning get_meaning () const override;

 private:
  bool m_edge_sense;
};

/* A concrete event subclass for the end of a consolidated run of
   CFG edges e.g. "...to here'.  */

class end_consolidated_cfg_edges_event : public checker_event
{
public:
  end_consolidated_cfg_edges_event (const event_loc_info &loc_info)
  : checker_event (EK_END_CONSOLIDATED_CFG_EDGES, loc_info)
  {
  }

  label_text get_desc (bool /*can_colorize*/) const final override
  {
    return label_text::borrow ("...to here");
  }
};

/* A concrete event subclass for describing an inlined call event
   e.g. "inlined call to 'callee' from 'caller'".  */

class inlined_call_event : public checker_event
{
public:
  inlined_call_event (location_t loc,
		      tree apparent_callee_fndecl,
		      tree apparent_caller_fndecl,
		      int actual_depth,
		      int stack_depth_adjustment)
  : checker_event (EK_INLINED_CALL,
		   event_loc_info (loc,
				   apparent_caller_fndecl,
				   actual_depth + stack_depth_adjustment)),
    m_apparent_callee_fndecl (apparent_callee_fndecl),
    m_apparent_caller_fndecl (apparent_caller_fndecl)
  {
    gcc_assert (LOCATION_BLOCK (loc) == NULL);
  }

  label_text get_desc (bool /*can_colorize*/) const final override;
  meaning get_meaning () const override;

private:
  tree m_apparent_callee_fndecl;
  tree m_apparent_caller_fndecl;
};

/* A concrete event subclass for a setjmp or sigsetjmp call.  */

class setjmp_event : public checker_event
{
public:
  setjmp_event (const event_loc_info &loc_info,
		const exploded_node *enode,
		const gcall *setjmp_call)
  : checker_event (EK_SETJMP, loc_info),
    m_enode (enode), m_setjmp_call (setjmp_call)
  {
  }

  label_text get_desc (bool can_colorize) const final override;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostic_event_id_t emission_id) final override;

private:
  const exploded_node *m_enode;
  const gcall *m_setjmp_call;
};

/* An abstract event subclass for rewinding from a longjmp to a setjmp
   (or siglongjmp to sigsetjmp).

   Base class for two from/to subclasses, showing the two halves of the
   rewind.  */

class rewind_event : public checker_event
{
public:
  tree get_longjmp_caller () const;
  tree get_setjmp_caller () const;
  const exploded_edge *get_eedge () const { return m_eedge; }

 protected:
  rewind_event (const exploded_edge *eedge,
		enum event_kind kind,
		const event_loc_info &loc_info,
		const rewind_info_t *rewind_info);
  const rewind_info_t *m_rewind_info;

 private:
  const exploded_edge *m_eedge;
};

/* A concrete event subclass for rewinding from a longjmp to a setjmp,
   showing the longjmp (or siglongjmp).  */

class rewind_from_longjmp_event : public rewind_event
{
public:
  rewind_from_longjmp_event (const exploded_edge *eedge,
			     const event_loc_info &loc_info,
			     const rewind_info_t *rewind_info)
  : rewind_event (eedge, EK_REWIND_FROM_LONGJMP, loc_info,
		  rewind_info)
  {
  }

  label_text get_desc (bool can_colorize) const final override;
};

/* A concrete event subclass for rewinding from a longjmp to a setjmp,
   showing the setjmp (or sigsetjmp).  */

class rewind_to_setjmp_event : public rewind_event
{
public:
  rewind_to_setjmp_event (const exploded_edge *eedge,
			  const event_loc_info &loc_info,
			  const rewind_info_t *rewind_info)
  : rewind_event (eedge, EK_REWIND_TO_SETJMP, loc_info,
		  rewind_info)
  {
  }

  label_text get_desc (bool can_colorize) const final override;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostic_event_id_t emission_id) final override;

private:
  diagnostic_event_id_t m_original_setjmp_event_id;
};

/* Concrete subclass of checker_event for use at the end of a path:
   a repeat of the warning message at the end of the path (perhaps with
   references to pertinent events that occurred on the way), at the point
   where the problem occurs.  */

class warning_event : public checker_event
{
public:
  warning_event (const event_loc_info &loc_info,
		 const exploded_node *enode,
		 const state_machine *sm,
		 tree var, state_machine::state_t state)
  : checker_event (EK_WARNING, loc_info),
    m_enode (enode),
    m_sm (sm), m_var (var), m_state (state)
  {
  }

  label_text get_desc (bool can_colorize) const final override;
  meaning get_meaning () const override;

  const exploded_node *get_exploded_node () const { return m_enode; }

private:
  const exploded_node *m_enode;
  const state_machine *m_sm;
  tree m_var;
  state_machine::state_t m_state;
};

} // namespace ana

#endif /* GCC_ANALYZER_CHECKER_EVENT_H */
