/* Operations within the code being analyzed.
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

#ifndef GCC_ANALYZER_OPS_H
#define GCC_ANALYZER_OPS_H

#include "except.h"
#include "gimple-walk.h"

namespace ana {

class operation;
  class control_flow_op;
  class call_and_return_op;
  class phis_for_edge_op;

class callsite_expr;

struct operation_context
{
  operation_context (exploded_graph &eg,
		     exploded_node &src_enode,
		     const superedge &sedge)
  : m_eg (eg),
    m_src_enode (src_enode),
    m_sedge (sedge)
  {
  }

  void DEBUG_FUNCTION dump () const;

  logger *get_logger () const;

  const extrinsic_state &get_ext_state () const;

  const program_point &
  get_initial_point () const;

  const program_state &
  get_initial_state () const;

  const supergraph &
  get_supergraph () const;

  program_point
  get_next_intraprocedural_point () const;

  void
  add_outcome (const program_point &dst_point,
	       program_state dst_state,
	       bool could_do_work,
	       uncertainty_t *uncertainty,
	       std::unique_ptr<custom_edge_info> info = nullptr);

  exploded_graph &m_eg;
  exploded_node &m_src_enode;
  const superedge &m_sedge;
};

/* Abstract base class for an operation along a superedge.  */

class operation
{
 public:
  // Discriminator for concrete subclasses
  enum kind
  {
    asm_stmt,
    assignment,
    predict_stmt,
    return_stmt,
    resx,
    cond_edge,
    goto_edge,
    switch_edge,
    eh_dispatch_try_edge,
    eh_dispatch_allowed_edge,
    phis,
    call_and_return
  };

  virtual ~operation () {}

  void
  dump () const;

  virtual std::unique_ptr<operation>
  clone () const = 0;

  virtual void
  print_as_edge_label (pretty_printer *pp, bool user_facing) const = 0;

  virtual bool
  defines_ssa_name_p (const_tree ssa_name) const = 0;

  virtual void
  walk_load_store_addr_ops (void *,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn) const = 0;

  virtual const gimple *
  maybe_get_stmt () const
  {
    return nullptr;
  }

  virtual void
  execute (operation_context &op_ctxt) const = 0;

  virtual bool
  execute_for_feasibility (const exploded_edge &,
			   feasibility_state &,
			   region_model_context *,
			   std::unique_ptr<rejected_constraint> */*out_rc*/) const
  {
    // no-op
    return true;
  }

  /* Is this op suitable for bulk-merging?
     It must have a single outcome, at the intraprocedural
     next point, with some state.  */
  virtual bool
  supports_bulk_merge_p () const = 0;
  virtual void
  update_state_for_bulk_merger (const program_state &,
				program_state &) const
  {
    /* Must be implemented for any subclasses that return true
       for supports_bulk_merge_p.  */
    gcc_unreachable ();
  }
  virtual void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const = 0;

  virtual const control_flow_op *
  dyn_cast_control_flow_op () const { return nullptr; }

  virtual const call_and_return_op *
  dyn_cast_call_and_return_op () const { return nullptr; }

  virtual const phis_for_edge_op *
  dyn_cast_phis_for_edge_op () const { return nullptr; }

  enum kind get_kind () const { return m_kind; }

protected:
  operation (enum kind kind_)
  : m_kind (kind_)
  {
  }

  static void
  handle_on_stmt_for_state_machines (operation_context &op_ctxt,
				     program_state &dst_state,
				     path_context *path_ctxt,
				     bool &unknown_side_effects,
				     const gimple &stmt);

private:
  enum kind m_kind;
};

/* Subclass for an operation representing a specific gimple stmt
   that isn't control flow.  */

class gimple_stmt_op : public operation
{
public:
  const gimple &get_stmt () const { return m_stmt; }

  void
  print_as_edge_label (pretty_printer *pp, bool user_facing) const override;

  bool
  defines_ssa_name_p (const_tree ssa_name) const final override;

  void
  walk_load_store_addr_ops (void *,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn) const final override;

  const gimple *
  maybe_get_stmt () const final override
  {
    return &m_stmt;
  }

  void
  execute (operation_context &op_ctxt) const override;

  void
  execute_on_state (operation_context &op_ctxt,
		    program_state dst_state) const;

  bool
  execute_for_feasibility (const exploded_edge &,
			   feasibility_state &,
			   region_model_context *,
			   std::unique_ptr<rejected_constraint> *out_rc) const override;

  virtual bool
  supports_bulk_merge_p () const override;

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const override;

protected:
  gimple_stmt_op (enum kind kind_, const gimple &stmt)
  : operation (kind_), m_stmt (stmt)
  {}

private:
  const gimple &m_stmt;
};

/* Various subclasses of gimple_stmt_op.  */

/* An operation subclass representing the effect of a GIMPLE_ASM stmt.  */

class gasm_op : public gimple_stmt_op
{
public:
  gasm_op (const gasm &asm_stmt)
  : gimple_stmt_op (kind::asm_stmt, asm_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<gasm_op> (get_gasm ());
  }

  const gasm &get_gasm () const
  {
    return *as_a <const gasm *> (&get_stmt ());
  }
};

/* An operation subclass representing the effect of a GIMPLE_ASSIGN stmt.  */

class gassign_op : public gimple_stmt_op
{
public:
  gassign_op (const gassign &assign_stmt)
  : gimple_stmt_op (kind::assignment, assign_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<gassign_op> (get_gassign ());
  }

  const gassign &get_gassign () const
  {
    return *as_a <const gassign *> (&get_stmt ());
  }
};

/* An operation subclass for a GIMPLE_PREDICT stmt.
   They have no effect on state, but can be useful for reconstructing
   where "return" statements were in the code the user originally wrote,
   to improve the reported locations in diagnostics.  */

class predict_op : public gimple_stmt_op
{
public:
  predict_op (const gimple &predict_stmt)
  : gimple_stmt_op (kind::predict_stmt, predict_stmt)
  {
    gcc_assert (predict_stmt.code == GIMPLE_PREDICT);
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<predict_op> (get_stmt ());
  }
};

/* An operation subclass representing both:
   (a) the effect of a GIMPLE_RETURN stmt: copying a value into the
   RESULT_DECL of the current frame, and
   (b) a hint when reporting diagnostics that this is the return
   path from the function (rather than say, throwing an exception).  */

class greturn_op : public gimple_stmt_op
{
public:
  greturn_op (const greturn &return_stmt)
  : gimple_stmt_op (kind::return_stmt, return_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<greturn_op> (get_greturn ());
  }

  void
  execute (operation_context &op_ctxt) const final override;

  bool
  execute_for_feasibility (const exploded_edge &,
			   feasibility_state &,
			   region_model_context *ctxt,
			   std::unique_ptr<rejected_constraint> *out_rc) const override;

  bool
  supports_bulk_merge_p () const final override
  {
    return false;
  }

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const final override;

  const greturn &get_greturn () const
  {
    return *as_a <const greturn *> (&get_stmt ());
  }

  tree get_retval () const
  {
    return gimple_return_retval (&get_greturn ());
  }
};

/* A concrete operation subclass representing the effect of a GIMPLE_CALL stmt.

   If the function is identified and has a known body, either simulate
   it interprocedurally by pushing a stack frame and transitioning to the
   callee, or simulate it intraprocedurally by replaying a summary of the
   effects of the call.

   If the function is identified but has an unknown body,
   simulate it intraprocedurally, either using a known_function
   subclass for precision, or following conservative rules that
   assume various side-effects.

   If the function is unidentified (for some kinds of dynamic calls),
   simulate it intraprocedurally, following conservative rules that
   assume various side-effects.

   In the various intraprocedural simulation cases, the exploded edge will
   correspond to the underlying superedge.

   In the interprocedural simulation case, the exploded edge will
   link two supernodes in different functions, and thus will require
   custom edge info.

   Various subclasses exist for handling awkward special cases,
   such as longjmp.  */

class call_and_return_op : public gimple_stmt_op
{
public:
  static std::unique_ptr<operation>
  make (const gcall &call_stmt);

  std::unique_ptr<operation>
  clone () const override
  {
    return std::make_unique<call_and_return_op> (get_gcall ());
  }

  const gcall &get_gcall () const
  {
    return *as_a <const gcall *> (&get_stmt ());
  }

  void
  execute (operation_context &op_ctxt) const override;

  bool
  supports_bulk_merge_p () const final override
  {
    return false;
  }

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const override;

  const call_and_return_op *
  dyn_cast_call_and_return_op () const final override { return this; }

  tree
  map_expr_from_caller_to_callee (tree callee_fndecl,
				  tree caller_expr,
				  callsite_expr *out) const;
  tree
  map_expr_from_callee_to_caller (tree callee_fndecl,
				  tree callee_expr,
				  callsite_expr *out) const;

  call_and_return_op (const gcall &call_stmt)
  : gimple_stmt_op (kind::call_and_return, call_stmt)
  {
  }

  const known_function *
  maybe_get_known_function (const call_details &cd) const;

private:
  cgraph_edge *
  get_any_cgraph_edge (operation_context &op_ctxt) const;

  void
  replay_call_summaries (operation_context &op_ctxt,
			 function &called_fn,
			 per_function_data &called_fn_data,
			 region_model_context *ctxt) const;

  void
  replay_call_summary (operation_context &op_ctxt,
		       function &called_fn,
		       call_summary &summary,
		       region_model_context *ctxt) const;

  tree
  get_arg_for_parm (tree callee_fndecl,
		    tree parm,
		    callsite_expr *out) const;
  tree
  get_parm_for_arg (tree callee_fndecl,
		    tree arg,
		    callsite_expr *out) const;
};

/* A call to one of the various __analyzer_dump* functions.
   These have no effect on state.  */

class dump_op : public call_and_return_op
{
public:
  enum dump_kind
  {
   state,
   sarif,
   dot,
   state_2
  };

  dump_op (const gcall &call_stmt, enum dump_kind dump_kind_)
  : call_and_return_op (call_stmt),
    m_dump_kind (dump_kind_)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<dump_op> (get_gcall (), m_dump_kind);
  }

  void
  execute (operation_context &op_ctxt) const final override;

private:
  enum dump_kind m_dump_kind;
};

class setjmp_op : public call_and_return_op
{
public:
  setjmp_op (const gcall &call_stmt)
  : call_and_return_op (call_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<setjmp_op> (get_gcall ());
  }

  void
  execute (operation_context &op_ctxt) const final override;

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const final override;
};

class longjmp_op : public call_and_return_op
{
public:
  longjmp_op (const gcall &call_stmt)
  : call_and_return_op (call_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<longjmp_op> (get_gcall ());
  }

  void
  execute (operation_context &op_ctxt) const final override;
};

class cxa_throw_op : public call_and_return_op
{
public:
  cxa_throw_op (const gcall &call_stmt, bool is_rethrow)
  : call_and_return_op (call_stmt),
    m_is_rethrow (is_rethrow)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<cxa_throw_op> (get_gcall (), m_is_rethrow);
  }

  void
  execute (operation_context &op_ctxt) const final override;

private:
  bool m_is_rethrow;
};

class resx_op : public gimple_stmt_op
{
public:
  resx_op (const gresx &resx_stmt)
  : gimple_stmt_op (kind::resx, resx_stmt)
  {
  }

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<resx_op> (get_gresx ());
  }

  const gresx &get_gresx () const
  {
    return *as_a <const gresx *> (&get_stmt ());
  }

  void
  execute (operation_context &op_ctxt) const final override;

  bool
  supports_bulk_merge_p () const final override
  {
    return false;
  }

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const final override;
};

/* An abstract subclass of operation representing the filtering effect on
   state of a gimple control-flow statement at the end of a BB, for
   a specific CFG out-edge from that BB.  */

class control_flow_op : public operation
{
public:
  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const override;

  bool
  defines_ssa_name_p (const_tree) const final override
  {
    return false;
  }

  void
  walk_load_store_addr_ops (void *,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn) const final override;

  const gimple *
  maybe_get_stmt () const final override
  {
    return &m_ctrlflow_stmt;
  }

  virtual label_text
  maybe_describe_condition (bool can_colorize) const;

  void
  execute (operation_context &op_ctxt) const final override;

  bool
  supports_bulk_merge_p () const final override
  {
    return false;
  }

  bool
  execute_for_feasibility (const exploded_edge &,
			   feasibility_state &,
			   region_model_context *,
			   std::unique_ptr<rejected_constraint> *out_rc) const override;

  const control_flow_op *
  dyn_cast_control_flow_op () const final override { return this; }

  ::edge get_cfg_edge () const { return m_cfg_edge; }
  int get_flags () const { return m_cfg_edge->flags; }
  int back_edge_p () const { return get_flags () & EDGE_DFS_BACK; }

  const gimple &get_ctrlflow_stmt () const { return m_ctrlflow_stmt; }

protected:
  control_flow_op (enum kind kind_,
		   ::edge cfg_edge,
		   const gimple &ctrlflow_stmt)
  : operation (kind_),
    m_cfg_edge (cfg_edge),
    m_ctrlflow_stmt (ctrlflow_stmt)
  {}

private:
  virtual bool
  apply_constraints (const superedge *sedge,
		     region_model &model,
		     region_model_context *ctxt,
		     std::unique_ptr<rejected_constraint> *out) const = 0;

  ::edge m_cfg_edge;
  const gimple &m_ctrlflow_stmt;
};

/* Concrete operation subclass representing filtering/applying state
   transitions on a specific CFG edge after a GIMPLE_COND stmt, either the
   "if (cond) is true" or the "if (cond) is false" branch.  */

class gcond_edge_op : public control_flow_op
{
public:
  gcond_edge_op (::edge cfg_edge,
		 const gcond &cond_stmt);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<gcond_edge_op> (get_cfg_edge (),
					    get_gcond ());
  }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  label_text
  maybe_describe_condition (bool can_colorize) const final override;

  const gcond &get_gcond () const
  {
    return *as_a <const gcond *> (&get_ctrlflow_stmt ());
  }

private:
  static label_text
  maybe_describe_condition (bool can_colorize,
			    tree lhs,
			    enum tree_code op,
			    tree rhs);
  static bool should_print_expr_p (tree expr);

  bool
  apply_constraints (const superedge *sedge,
		     region_model &model,
		     region_model_context *ctxt,
		     std::unique_ptr<rejected_constraint> *out)
    const final override;

  bool m_true_value;
};

/* Concrete operation subclass representing filtering/applying state
   transitions on a specific CFG edge after a GIMPLE_GOTO stmt, thus
   handling computed gotos.  */

class ggoto_edge_op : public control_flow_op
{
public:
  ggoto_edge_op (::edge cfg_edge,
		 const ggoto &goto_stmt,
		 tree dst_label);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<ggoto_edge_op> (get_cfg_edge (),
					    get_ggoto (),
					    m_dst_label);
  }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  label_text
  maybe_describe_condition (bool can_colorize) const final override;

  const ggoto &get_ggoto () const
  {
    return *as_a <const ggoto *> (&get_ctrlflow_stmt ());
  }

private:
  bool
  apply_constraints (const superedge *sedge,
		     region_model &model,
		     region_model_context *ctxt,
		     std::unique_ptr<rejected_constraint> *out)
    const final override;

  tree m_dst_label;
};

/* Concrete operation subclass representing filtering/applying state
   transitions on a specific CFG edge after a GIMPLE_SWITCH stmt, thus
   handling a cluster of cases/default value.  */

class switch_case_op : public control_flow_op
{
 public:
  switch_case_op (function &fun,
		  ::edge cfg_edge,
		  const gswitch &switch_stmt,
		  bounded_ranges_manager &mgr);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<switch_case_op> (*this);
  }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  bool implicitly_created_default_p () const;

  const gswitch &get_gswitch () const
  {
    return *as_a <const gswitch *> (&get_ctrlflow_stmt ());
  }

 private:
  bool
  apply_constraints (const superedge *sedge,
		     region_model &model,
		     region_model_context *ctxt,
		     std::unique_ptr<rejected_constraint> *out)
    const final override;

  std::vector<tree> m_case_labels;
  const bounded_ranges *m_all_cases_ranges;
};

/* Abstract subclass for edges from eh_dispatch statements.  */

class eh_dispatch_edge_op : public control_flow_op
{
public:
  static std::unique_ptr<eh_dispatch_edge_op>
  make (supernode *src,
	supernode *dest,
	::edge cfg_edge,
	const geh_dispatch &geh_dispatch_stmt);

  const geh_dispatch &
  get_geh_dispatch () const
  {
    return *as_a <const geh_dispatch *> (&get_ctrlflow_stmt ());
  }

  eh_region
  get_eh_region () const { return m_eh_region; }

protected:
  eh_dispatch_edge_op (supernode *src_snode,
		       enum kind kind_,
		       ::edge cfg_edge,
		       const geh_dispatch &geh_dispatch_stmt,
		       eh_region eh_reg);

  supernode *get_src_snode () const { return m_src_snode; }

private:
  bool
  apply_constraints (const superedge *sedge,
		     region_model &model,
		     region_model_context *ctxt,
		     std::unique_ptr<rejected_constraint> *out)
    const final override;

  virtual bool
  apply_eh_constraints (const superedge *sedge,
			region_model &model,
			region_model_context *ctxt,
			tree exception_type,
			std::unique_ptr<rejected_constraint> *out) const = 0;

  supernode *m_src_snode;
  eh_region m_eh_region;
};

/* Concrete operation for edges from an eh_dispatch statement
   for ERT_TRY regions.  */

class eh_dispatch_try_edge_op : public eh_dispatch_edge_op
{
public:
  eh_dispatch_try_edge_op (supernode *src_snode,
			   ::edge cfg_edge,
			   const geh_dispatch &geh_dispatch_stmt,
			   eh_region eh_reg,
			   eh_catch ehc);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<eh_dispatch_try_edge_op> (*this);
  }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const final override;

private:
  bool
  apply_eh_constraints (const superedge *sedge,
			region_model &model,
			region_model_context *ctxt,
			tree exception_type,
			std::unique_ptr<rejected_constraint> *out)
    const final override;

  eh_catch m_eh_catch;
};

/* Concrete operation for edges from an eh_dispatch statement
   for ERT_ALLOWED_EXCEPTIONS regions.  */

class eh_dispatch_allowed_edge_op : public eh_dispatch_edge_op
{
public:
  enum eh_kind
  {
    expected,
    unexpected
  };

  eh_dispatch_allowed_edge_op (supernode *src_snode,
			       supernode *dst_snode,
			       ::edge cfg_edge,
			       const geh_dispatch &geh_dispatch_stmt,
			       eh_region eh_reg);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<eh_dispatch_allowed_edge_op> (*this);
  }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  enum eh_kind get_eh_kind () const { return m_kind; }

private:
  bool
  apply_eh_constraints (const superedge *sedge,
			region_model &model,
			region_model_context *ctxt,
			tree exception_type,
			std::unique_ptr<rejected_constraint> *out)
    const final override;

  enum eh_kind m_kind;
};

/* Concrete operation subclass representing the state transition
   for simultaneously handling all of the phi nodes at the entry to a BB,
   after following a specific CFG in-edge.
   Note that this covers multiple gimple stmts: all of the gphi stmts
   at a basic block entry (albeit for just one in-edge).
   This can be thought of as handling one column of the entries in the
   phi nodes of a BB (for a specific in-edge).
   We ignore MEM entries, and discard phi nodes purely affecting them.  */

class phis_for_edge_op : public operation
{
public:
  /* A "dst=src;" pair within a phi node.  */
  struct pair
  {
    tree m_dst;
    tree m_src;
  };

  static std::unique_ptr<operation>
  maybe_make (::edge cfg_in_edge);

  std::unique_ptr<operation>
  clone () const final override
  {
    return std::make_unique<phis_for_edge_op> (*this);
  }

  phis_for_edge_op (std::vector<pair> &&pairs);

  const phis_for_edge_op *
  dyn_cast_phis_for_edge_op () const final override { return this; }

  void
  print_as_edge_label (pretty_printer *pp,
		       bool user_facing) const final override;

  bool
  defines_ssa_name_p (const_tree ssa_name) const final override;

  void
  walk_load_store_addr_ops (void *,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn,
			    walk_stmt_load_store_addr_fn) const final override;
  void
  execute (operation_context &op_ctxt) const final override;

  bool
  execute_for_feasibility (const exploded_edge &,
			   feasibility_state &,
			   region_model_context *,
			   std::unique_ptr<rejected_constraint> *out_rc) const override;

  bool
  supports_bulk_merge_p () const final override
  {
    return true;
  }
  void
  update_state_for_bulk_merger (const program_state &src_state,
				program_state &dst_state) const final override;

  void
  add_any_events_for_eedge (const exploded_edge &eedge,
			    checker_path &out_path) const final override;

  const std::vector<pair> &get_pairs () const { return m_pairs; }

private:
  static std::vector<pair>
  get_pairs_for_phi_along_in_edge (::edge cfg_in_edge);

  void
  update_state (const program_state &src_state,
		program_state &dst_state,
		region_model_context *ctxt) const;

  std::vector<pair> m_pairs;
};

} // namespace ana

#endif /* GCC_ANALYZER_OPS_H */
