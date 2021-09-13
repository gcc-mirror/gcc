/* Subclasses of diagnostic_path and diagnostic_event for analyzer diagnostics.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_CHECKER_PATH_H
#define GCC_ANALYZER_CHECKER_PATH_H

namespace ana {

/* An enum for discriminating between the concrete subclasses of
   checker_event.  */

enum event_kind
{
  EK_DEBUG,
  EK_CUSTOM,
  EK_STMT,
  EK_FUNCTION_ENTRY,
  EK_STATE_CHANGE,
  EK_START_CFG_EDGE,
  EK_END_CFG_EDGE,
  EK_CALL_EDGE,
  EK_RETURN_EDGE,
  EK_START_CONSOLIDATED_CFG_EDGES,
  EK_END_CONSOLIDATED_CFG_EDGES,
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
  checker_event (enum event_kind kind,
		 location_t loc, tree fndecl, int depth)
    : m_kind (kind), m_loc (loc), m_fndecl (fndecl), m_depth (depth),
      m_pending_diagnostic (NULL), m_emission_id ()
  {
  }

  /* Implementation of diagnostic_event.  */

  location_t get_location () const FINAL OVERRIDE { return m_loc; }
  tree get_fndecl () const FINAL OVERRIDE { return m_fndecl; }
  int get_stack_depth () const FINAL OVERRIDE { return m_depth; }

  /* Additional functionality.  */

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

  void set_location (location_t loc) { m_loc = loc; }

 public:
  const enum event_kind m_kind;
 protected:
  location_t m_loc;
  tree m_fndecl;
  int m_depth;
  pending_diagnostic *m_pending_diagnostic;
  diagnostic_event_id_t m_emission_id; // only set once all pruning has occurred
};

/* A concrete event subclass for a purely textual event, for use in
   debugging path creation and filtering.  */

class debug_event : public checker_event
{
public:
  debug_event (location_t loc, tree fndecl, int depth,
	      const char *desc)
  : checker_event (EK_DEBUG, loc, fndecl, depth),
    m_desc (xstrdup (desc))
  {
  }
  ~debug_event ()
  {
    free (m_desc);
  }

  label_text get_desc (bool) const FINAL OVERRIDE;

private:
  char *m_desc;
};

/* An abstract event subclass for custom events.  These are not filtered,
   as they are likely to be pertinent to the diagnostic.  */

class custom_event : public checker_event
{
protected:
  custom_event (location_t loc, tree fndecl, int depth)
  : checker_event (EK_CUSTOM, loc, fndecl, depth)
  {
  }
};

/* A concrete custom_event subclass with a precanned message.  */

class precanned_custom_event : public custom_event
{
public:
  precanned_custom_event (location_t loc, tree fndecl, int depth,
			  const char *desc)
  : custom_event (loc, fndecl, depth),
    m_desc (xstrdup (desc))
  {
  }
  ~precanned_custom_event ()
  {
    free (m_desc);
  }

  label_text get_desc (bool) const FINAL OVERRIDE;

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

  label_text get_desc (bool) const FINAL OVERRIDE;

  const gimple * const m_stmt;
  const program_state m_dst_state;
};

/* An event subclass describing the entry to a function.  */

class function_entry_event : public checker_event
{
public:
  function_entry_event (location_t loc, tree fndecl, int depth)
  : checker_event (EK_FUNCTION_ENTRY, loc, fndecl, depth)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  bool is_function_entry_p () const FINAL OVERRIDE { return true; }
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
		      const program_state &dst_state);

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  function *get_dest_function () const
  {
    return m_dst_state.get_current_function ();
  }

  const supernode *m_node;
  const gimple *m_stmt;
  const state_machine &m_sm;
  const svalue *m_sval;
  state_machine::state_t m_from;
  state_machine::state_t m_to;
  const svalue *m_origin;
  program_state m_dst_state;
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
		   location_t loc, tree fndecl, int depth);

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
  const cfg_superedge& get_cfg_superedge () const;

 protected:
  cfg_edge_event (enum event_kind kind, const exploded_edge &eedge,
		  location_t loc, tree fndecl, int depth);
};

/* A concrete event subclass for the start of a CFG edge
   e.g. "following 'false' branch...'.  */

class start_cfg_edge_event : public cfg_edge_event
{
public:
  start_cfg_edge_event (const exploded_edge &eedge,
			location_t loc, tree fndecl, int depth)
  : cfg_edge_event (EK_START_CFG_EDGE, eedge, loc, fndecl, depth)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

 private:
  label_text maybe_describe_condition (bool can_colorize) const;

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
		      location_t loc, tree fndecl, int depth)
  : cfg_edge_event (EK_END_CFG_EDGE, eedge, loc, fndecl, depth)
  {
  }

  label_text get_desc (bool /*can_colorize*/) const FINAL OVERRIDE
  {
    return label_text::borrow ("...to here");
  }
};

/* A concrete event subclass for an interprocedural call.  */

class call_event : public superedge_event
{
public:
  call_event (const exploded_edge &eedge,
	      location_t loc, tree fndecl, int depth);

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  bool is_call_p () const FINAL OVERRIDE;

  const supernode *m_src_snode;
  const supernode *m_dest_snode;
};

/* A concrete event subclass for an interprocedural return.  */

class return_event : public superedge_event
{
public:
  return_event (const exploded_edge &eedge,
		location_t loc, tree fndecl, int depth);

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  bool is_return_p () const FINAL OVERRIDE;

  const supernode *m_src_snode;
  const supernode *m_dest_snode;
};

/* A concrete event subclass for the start of a consolidated run of CFG
   edges all either TRUE or FALSE e.g. "following 'false' branch...'.  */

class start_consolidated_cfg_edges_event : public checker_event
{
public:
  start_consolidated_cfg_edges_event (location_t loc, tree fndecl, int depth,
				      bool edge_sense)
  : checker_event (EK_START_CONSOLIDATED_CFG_EDGES, loc, fndecl, depth),
    m_edge_sense (edge_sense)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

 private:
  bool m_edge_sense;
};

/* A concrete event subclass for the end of a consolidated run of
   CFG edges e.g. "...to here'.  */

class end_consolidated_cfg_edges_event : public checker_event
{
public:
  end_consolidated_cfg_edges_event (location_t loc, tree fndecl, int depth)
  : checker_event (EK_END_CONSOLIDATED_CFG_EDGES, loc, fndecl, depth)
  {
  }

  label_text get_desc (bool /*can_colorize*/) const FINAL OVERRIDE
  {
    return label_text::borrow ("...to here");
  }
};

/* A concrete event subclass for a setjmp or sigsetjmp call.  */

class setjmp_event : public checker_event
{
public:
  setjmp_event (location_t loc, const exploded_node *enode,
		tree fndecl, int depth, const gcall *setjmp_call)
  : checker_event (EK_SETJMP, loc, fndecl, depth),
    m_enode (enode), m_setjmp_call (setjmp_call)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostic_event_id_t emission_id) FINAL OVERRIDE;

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
		location_t loc, tree fndecl, int depth,
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
			     location_t loc, tree fndecl, int depth,
			     const rewind_info_t *rewind_info)
  : rewind_event (eedge, EK_REWIND_FROM_LONGJMP, loc, fndecl, depth,
		  rewind_info)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;
};

/* A concrete event subclass for rewinding from a longjmp to a setjmp,
   showing the setjmp (or sigsetjmp).  */

class rewind_to_setjmp_event : public rewind_event
{
public:
  rewind_to_setjmp_event (const exploded_edge *eedge,
			  location_t loc, tree fndecl, int depth,
			  const rewind_info_t *rewind_info)
  : rewind_event (eedge, EK_REWIND_TO_SETJMP, loc, fndecl, depth,
		  rewind_info)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

  void prepare_for_emission (checker_path *path,
			     pending_diagnostic *pd,
			     diagnostic_event_id_t emission_id) FINAL OVERRIDE;

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
  warning_event (location_t loc, tree fndecl, int depth,
		 const state_machine *sm,
		 tree var, state_machine::state_t state)
  : checker_event (EK_WARNING, loc, fndecl, depth),
    m_sm (sm), m_var (var), m_state (state)
  {
  }

  label_text get_desc (bool can_colorize) const FINAL OVERRIDE;

private:
  const state_machine *m_sm;
  tree m_var;
  state_machine::state_t m_state;
};

/* Subclass of diagnostic_path for analyzer diagnostics.  */

class checker_path : public diagnostic_path
{
public:
  checker_path () : diagnostic_path () {}

  /* Implementation of diagnostic_path vfuncs.  */

  unsigned num_events () const FINAL OVERRIDE
  {
    return m_events.length ();
  }

  const diagnostic_event & get_event (int idx) const FINAL OVERRIDE
  {
    return *m_events[idx];
  }

  checker_event *get_checker_event (int idx)
  {
    return m_events[idx];
  }

  void dump (pretty_printer *pp) const;
  void debug () const;

  void maybe_log (logger *logger, const char *desc) const;

  void add_event (checker_event *event)
  {
    m_events.safe_push (event);
  }

  void delete_event (int idx)
  {
    checker_event *event = m_events[idx];
    m_events.ordered_remove (idx);
    delete event;
  }

  void delete_events (unsigned start_idx, unsigned len)
  {
    for (unsigned i = start_idx; i < start_idx + len; i++)
      delete m_events[i];
    m_events.block_remove (start_idx, len);
  }

  void replace_event (unsigned idx, checker_event *new_event)
  {
    delete m_events[idx];
    m_events[idx] = new_event;
  }

  void add_final_event (const state_machine *sm,
			const exploded_node *enode, const gimple *stmt,
			tree var, state_machine::state_t state);

  /* After all event-pruning, a hook for notifying each event what
     its ID will be.  The events are notified in order, allowing
     for later events to refer to the IDs of earlier events in
     their descriptions.  */
  void prepare_for_emission (pending_diagnostic *pd)
  {
    checker_event *e;
    int i;
    FOR_EACH_VEC_ELT (m_events, i, e)
      e->prepare_for_emission (this, pd, diagnostic_event_id_t (i));
  }

  void fixup_locations (pending_diagnostic *pd);

  void record_setjmp_event (const exploded_node *enode,
			    diagnostic_event_id_t setjmp_emission_id)
  {
    m_setjmp_event_ids.put (enode, setjmp_emission_id);
  }

  bool get_setjmp_event (const exploded_node *enode,
			 diagnostic_event_id_t *out_emission_id)
  {
    if (diagnostic_event_id_t *emission_id = m_setjmp_event_ids.get (enode))
      {
	*out_emission_id = *emission_id;
	return true;
      }
    return false;
  }

  bool cfg_edge_pair_at_p (unsigned idx) const;

private:
  DISABLE_COPY_AND_ASSIGN(checker_path);

  /* The events that have occurred along this path.  */
  auto_delete_vec<checker_event> m_events;

  /* During prepare_for_emission (and after), the setjmp_event for each
     exploded_node *, so that rewind events can refer to them in their
     descriptions.  */
  hash_map <const exploded_node *, diagnostic_event_id_t> m_setjmp_event_ids;
};

} // namespace ana

#endif /* GCC_ANALYZER_CHECKER_PATH_H */
