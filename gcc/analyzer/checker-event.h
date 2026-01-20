/* Subclasses of diagnostics::paths::event for analyzer diagnostics.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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
#include "analyzer/event-loc-info.h"
#include "diagnostics/digraphs.h"

namespace ana {

/* An enum for discriminating between the concrete subclasses of
   checker_event.  */

enum class event_kind
{
  debug,
  custom,
  stmt,
  region_creation,
  function_entry,
  state_change,
  start_cfg_edge,
  end_cfg_edge,
  catch_,
  call_,
  return_,
  start_consolidated_cfg_edges,
  end_consolidated_cfg_edges,
  inlined_call,
  setjmp_,
  rewind_from_longjmp,
  rewind_to_setjmp,
  throw_,
  unwind,
  warning
};

extern const char *event_kind_to_string (enum event_kind ek);

/* Event subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with event_kinds shown for the concrete subclasses):

   diagnostics::paths::event
     checker_event
       debug_event (event_kind::debug)
       custom_event (event_kind::custom)
	 precanned_custom_event
       statement_event (event_kind::stmt)
       region_creation_event (event_kind::region_creation)
       function_entry_event (event_kind::function_entry)
       state_change_event (event_kind::state_change)
       superedge_event
         cfg_edge_event
	   start_cfg_edge_event (event_kind::start_cfg_edge)
	   end_cfg_edge_event (event_kind::end_cfg_edge)
	   catch_cfg_edge_event (event_kind::catch_cfg_edge)
	 call_event (event_kind::call_)
       return_event (event_kind::return_)
       start_consolidated_cfg_edges_event (event_kind::start_consolidated_cfg_edges)
       end_consolidated_cfg_edges_event (event_kind::end_consolidated_cfg_edges)
       inlined_call_event (event_kind::inlined_call)
       setjmp_event (event_kind::setjmp_)
       rewind_event
         rewind_from_longjmp_event (event_kind::rewind_from_longjmp)
	 rewind_to_setjmp_event (event_kind::rewind_to_setjmp)
       throw_event (event_kind:throw_)
	 explicit_throw_event
	 throw_from_call_to_external_fn_event
       unwind_event (event_kind::unwind)
       warning_event (event_kind::warning).  */

/* Abstract subclass of diagnostics::paths::event; the base class for use in
   checker_path (the analyzer's diagnostics::paths::path subclass).  */

class checker_event : public diagnostics::paths::event
{
public:
  /* Implementation of diagnostics::paths::event.  */

  location_t get_location () const final override { return m_loc; }
  int get_stack_depth () const final override { return m_effective_depth; }
  diagnostics::logical_locations::key
  get_logical_location () const final override
  {
    return m_logical_loc;
  }
  meaning get_meaning () const override;
  bool connect_to_next_event_p () const override { return false; }
  diagnostics::paths::thread_id_t get_thread_id () const final override
  {
    return 0;
  }

  void
  maybe_add_sarif_properties (diagnostics::sarif_builder &,
			      diagnostics::sarif_object &thread_flow_loc_obj)
    const override;

  /* Additional functionality.  */
  enum event_kind get_kind () const { return m_kind; }
  tree get_fndecl () const { return m_effective_fndecl; }

  int get_original_stack_depth () const { return m_original_depth; }

  virtual void prepare_for_emission (checker_path *,
				     pending_diagnostic *pd,
				     diagnostics::paths::event_id_t emission_id);
  virtual bool is_call_p () const { return false; }
  virtual bool is_function_entry_p () const  { return false; }
  virtual bool is_return_p () const  { return false; }

  std::unique_ptr<diagnostics::digraphs::digraph>
  maybe_make_diagnostic_state_graph (bool debug) const final override;

  virtual const program_state *
  get_program_state () const { return nullptr; }

  /* For use with %@.  */
  const diagnostics::paths::event_id_t *get_id_ptr () const
  {
    return &m_emission_id;
  }

  void dump (pretty_printer *pp) const;
  void debug () const;

  void set_location (location_t loc) { m_loc = loc; }

protected:
  checker_event (enum event_kind kind,
		 const event_loc_info &loc_info);

 private:
  const checker_path *m_path;
  const enum event_kind m_kind;
 protected:
  location_t m_loc;
  tree m_original_fndecl;
  tree m_effective_fndecl;
  int m_original_depth;
  int m_effective_depth;
  pending_diagnostic *m_pending_diagnostic;
  diagnostics::paths::event_id_t m_emission_id; // only set once all pruning has occurred
  diagnostics::logical_locations::key m_logical_loc;
};

/* A concrete event subclass for a purely textual event, for use in
   debugging path creation and filtering.  */

class debug_event : public checker_event
{
public:

  debug_event (const event_loc_info &loc_info,
	       const char *desc)
  : checker_event (event_kind::debug, loc_info),
    m_desc (xstrdup (desc))
  {
  }
  ~debug_event ()
  {
    free (m_desc);
  }

  void print_desc (pretty_printer &) const final override;

private:
  char *m_desc;
};

/* An abstract event subclass for custom events.  These are not filtered,
   as they are likely to be pertinent to the diagnostic.  */

class custom_event : public checker_event
{
protected:
  custom_event (const event_loc_info &loc_info)
  : checker_event (event_kind::custom, loc_info)
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

  void print_desc (pretty_printer &) const final override;

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

  void print_desc (pretty_printer &) const final override;

  const program_state *
  get_program_state () const final override
  {
    return &m_dst_state;
  }

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

  void print_desc (pretty_printer &pp) const final override;

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

  void print_desc (pretty_printer &pp) const final override;

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

  void print_desc (pretty_printer &pp) const final override;

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

  void print_desc (pretty_printer &pp) const final override;

private:
  const region *m_reg;
  tree m_capacity;
};

/* An event subclass describing the entry to a function.  */

class function_entry_event : public checker_event
{
public:
  function_entry_event (const event_loc_info &loc_info,
			const program_state &state)
  : checker_event (event_kind::function_entry, loc_info),
    m_state (state)
  {
  }

  function_entry_event (const program_point &dst_point,
			const program_state &state);

  void print_desc (pretty_printer &pp) const override;
  meaning get_meaning () const override;

  bool is_function_entry_p () const final override { return true; }

  const program_state *
  get_program_state () const final override
  {
    return &m_state;
  }

private:
  const program_state &m_state;
};

/* Subclass of checker_event describing a state change.  */

class state_change_event : public checker_event
{
public:
  state_change_event (const event_loc_info &loc_info,
		      const gimple *stmt,
		      const state_machine &sm,
		      const svalue *sval,
		      state_machine::state_t from,
		      state_machine::state_t to,
		      const svalue *origin,
		      const program_state &dst_state,
		      const exploded_node *enode);

  void print_desc (pretty_printer &pp) const final override;
  meaning get_meaning () const override;

  const program_state *
  get_program_state () const final override
  {
    return &m_dst_state;
  }

  const function *get_dest_function () const
  {
    return m_dst_state.get_current_function ();
  }

  const exploded_node *get_exploded_node () const { return m_enode; }

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
  void
  maybe_add_sarif_properties (diagnostics::sarif_builder &,
			      diagnostics::sarif_object &thread_flow_loc_obj)
    const override;

  bool should_filter_p (int verbosity) const;

  const program_state *
  get_program_state () const override;

  virtual const call_and_return_op *
  get_call_and_return_op () const;

 protected:
  superedge_event (enum event_kind kind, const exploded_edge &eedge,
		   const event_loc_info &loc_info);

 public:
  const exploded_edge &m_eedge;
  const superedge *m_sedge;
};

/* An abstract event subclass for when a CFG edge is followed; it has two
   subclasses, representing the start of the edge and the end of the
   edge, which come in pairs.  */

class cfg_edge_event : public superedge_event
{
public:
  meaning get_meaning () const override;

  ::edge get_cfg_edge () const;

  bool maybe_get_edge_sense (bool *out) const;

 protected:
  cfg_edge_event (enum event_kind kind,
		  const exploded_edge &eedge,
		  const event_loc_info &loc_info,
		  const control_flow_op *op);

  const control_flow_op *m_op;
};

/* A concrete event subclass for the start of a CFG edge
   e.g. "following 'false' branch...'.  */

class start_cfg_edge_event : public cfg_edge_event
{
public:
  start_cfg_edge_event (const exploded_edge &eedge,
			const event_loc_info &loc_info,
			const control_flow_op *op)
  : cfg_edge_event (event_kind::start_cfg_edge, eedge, loc_info, op)
  {
  }

  void print_desc (pretty_printer &pp) const override;
  bool connect_to_next_event_p () const final override { return true; }

private:
  static bool should_print_expr_p (tree);
};

/* A concrete event subclass for the end of a CFG edge
   e.g. "...to here'.  */

class end_cfg_edge_event : public cfg_edge_event
{
public:
  end_cfg_edge_event (const exploded_edge &eedge,
		      const event_loc_info &loc_info,
		      const control_flow_op *op)
  : cfg_edge_event (event_kind::end_cfg_edge, eedge, loc_info, op)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, "...to here");
  }
};

/* A concrete event subclass for catching an exception
   e.g. "...catching 'struct io_error' here".  */

class catch_cfg_edge_event : public cfg_edge_event
{
public:
  catch_cfg_edge_event (const exploded_edge &eedge,
			const event_loc_info &loc_info,
			const control_flow_op &op,
			tree type)
  : cfg_edge_event (event_kind::catch_, eedge, loc_info, &op),
    m_type (type)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    if (m_type)
      pp_printf (&pp, "...catching exception of type %qT here", m_type);
    else
      pp_string (&pp, "...catching exception here");
  }

  meaning get_meaning () const override;

private:
  tree m_type;
};

struct critical_state
{
  critical_state ()
  : m_var (NULL_TREE),
    m_state (nullptr)
  {
  }
  critical_state (tree var, state_machine::state_t state)
  : m_var (var),
    m_state (state)
  {
  }

  tree m_var;
  state_machine::state_t m_state;
};

/* A concrete event subclass for an interprocedural call.  */

class call_event : public superedge_event
{
public:
  call_event (const exploded_edge &eedge,
	      const event_loc_info &loc_info);

  void print_desc (pretty_printer &pp) const override;
  meaning get_meaning () const override;

  bool is_call_p () const final override;

  const program_state *
  get_program_state () const final override;

  /* Mark this edge event as being either an interprocedural call or
     return in which VAR is in STATE, and that this is critical to the
     diagnostic (so that print_desc can attempt to get a better description
     from any pending_diagnostic).  */
  void record_critical_state (tree var, state_machine::state_t state)
  {
    m_critical_state = critical_state (var, state);
  }

protected:
  tree get_caller_fndecl () const;
  tree get_callee_fndecl () const;

  const supernode *m_src_snode;
  const supernode *m_dest_snode;
  critical_state m_critical_state;
};

/* A concrete event subclass for an interprocedural return.  */

class return_event : public checker_event
{
public:
  return_event (const exploded_edge &eedge,
		const event_loc_info &loc_info);

  void print_desc (pretty_printer &pp) const final override;
  meaning get_meaning () const override;

  bool is_return_p () const final override;

  const call_and_return_op *
  get_call_and_return_op () const
  {
    return m_call_and_return_op;
  }

  const program_state *
  get_program_state () const override;

  /* Mark this edge event as being either an interprocedural call or
     return in which VAR is in STATE, and that this is critical to the
     diagnostic (so that print_desc can attempt to get a better description
     from any pending_diagnostic).  */
  void record_critical_state (tree var, state_machine::state_t state)
  {
    m_critical_state = critical_state (var, state);
  }

  const exploded_edge &m_eedge;
  const supernode *m_src_snode;
  const supernode *m_dest_snode;
  const call_and_return_op *m_call_and_return_op;
  critical_state m_critical_state;
};

/* A concrete event subclass for the start of a consolidated run of CFG
   edges all either TRUE or FALSE e.g. "following 'false' branch...'.  */

class start_consolidated_cfg_edges_event : public checker_event
{
public:
  start_consolidated_cfg_edges_event (const event_loc_info &loc_info,
				      bool edge_sense)
  : checker_event (event_kind::start_consolidated_cfg_edges, loc_info),
    m_edge_sense (edge_sense)
  {
  }

  void print_desc (pretty_printer &pp) const final override;
  meaning get_meaning () const override;
  bool connect_to_next_event_p () const final override { return true; }

 private:
  bool m_edge_sense;
};

/* A concrete event subclass for the end of a consolidated run of
   CFG edges e.g. "...to here'.  */

class end_consolidated_cfg_edges_event : public checker_event
{
public:
  end_consolidated_cfg_edges_event (const event_loc_info &loc_info)
  : checker_event (event_kind::end_consolidated_cfg_edges, loc_info)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, "...to here");
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
  : checker_event (event_kind::inlined_call,
		   event_loc_info (loc,
				   apparent_caller_fndecl,
				   actual_depth + stack_depth_adjustment)),
    m_apparent_callee_fndecl (apparent_callee_fndecl),
    m_apparent_caller_fndecl (apparent_caller_fndecl)
  {
    gcc_assert (LOCATION_BLOCK (loc) == NULL);
  }

  void print_desc (pretty_printer &) const final override;
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
		const gcall &setjmp_call)
  : checker_event (event_kind::setjmp_, loc_info),
    m_enode (enode), m_setjmp_call (setjmp_call)
  {
  }

  void print_desc (pretty_printer &pp) const final override;

  meaning get_meaning () const override;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostics::paths::event_id_t emission_id) final override;

private:
  const exploded_node *m_enode;
  const gcall &m_setjmp_call;
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

  meaning get_meaning () const override;

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
  : rewind_event (eedge, event_kind::rewind_from_longjmp, loc_info,
		  rewind_info)
  {
  }

  void print_desc (pretty_printer &pp) const final override;
};

/* A concrete event subclass for rewinding from a longjmp to a setjmp,
   showing the setjmp (or sigsetjmp).  */

class rewind_to_setjmp_event : public rewind_event
{
public:
  rewind_to_setjmp_event (const exploded_edge *eedge,
			  const event_loc_info &loc_info,
			  const rewind_info_t *rewind_info)
  : rewind_event (eedge, event_kind::rewind_to_setjmp, loc_info,
		  rewind_info)
  {
  }

  void print_desc (pretty_printer &pp) const final override;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostics::paths::event_id_t emission_id) final override;

private:
  diagnostics::paths::event_id_t m_original_setjmp_event_id;
};

/* An abstract subclass for throwing/rethrowing an exception.  */

class throw_event : public checker_event
{
public:
  throw_event (const event_loc_info &loc_info,
	       const exploded_node *enode,
	       const gcall &throw_call)
  : checker_event (event_kind::throw_, loc_info),
    m_enode (enode),
    m_throw_call (throw_call)
  {
  }

  meaning get_meaning () const override;

protected:
  const exploded_node *m_enode;
  const gcall &m_throw_call;
};

/* A concrete event subclass for an explicit "throw EXC;"
   or "throw;"  (actually, a call to __cxa_throw or __cxa_rethrow).  */

class explicit_throw_event : public throw_event
{
public:
  explicit_throw_event (const event_loc_info &loc_info,
			const exploded_node *enode,
			const gcall &throw_call,
			tree type,
			bool is_rethrow)
  : throw_event (loc_info, enode, throw_call),
    m_type (type),
    m_is_rethrow (is_rethrow)
  {
  }

  void print_desc (pretty_printer &pp) const final override;

private:
  tree m_type;
  bool m_is_rethrow;
};

/* A concrete event subclass for an exception being thrown
   from within a call to a function we don't have the body of,
   or where we don't know what function was called.  */

class throw_from_call_to_external_fn_event : public throw_event
{
public:
  throw_from_call_to_external_fn_event (const event_loc_info &loc_info,
					const exploded_node *enode,
					const gcall &throw_call,
					tree fndecl)
  : throw_event (loc_info, enode, throw_call),
    m_fndecl (fndecl)
  {
  }

  void print_desc (pretty_printer &pp) const final override;

private:
  tree m_fndecl;
};

/* A concrete event subclass for unwinding a stack frame when
   processing an exception.  */

class unwind_event : public checker_event
{
public:
  unwind_event (const event_loc_info &loc_info)
  : checker_event (event_kind::unwind, loc_info),
    m_num_frames (1)
  {
  }

  meaning get_meaning () const override;

  void print_desc (pretty_printer &pp) const final override;

  int m_num_frames;
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
		 tree var, state_machine::state_t state,
		 const program_state *program_state_ = nullptr)
  : checker_event (event_kind::warning, loc_info),
    m_enode (enode),
    m_sm (sm), m_var (var), m_state (state)
  {
    if (program_state_)
      m_program_state = std::make_unique<program_state> (*program_state_);
  }

  void print_desc (pretty_printer &pp) const final override;
  meaning get_meaning () const override;

  const program_state *
  get_program_state () const final override;

  const exploded_node *get_exploded_node () const { return m_enode; }

private:
  const exploded_node *m_enode;
  const state_machine *m_sm;
  tree m_var;
  state_machine::state_t m_state;
  /* Optional copy of program state, for when this is different from
     m_enode's state:  */
  std::unique_ptr<program_state> m_program_state;
};

} // namespace ana

#endif /* GCC_ANALYZER_CHECKER_EVENT_H */
