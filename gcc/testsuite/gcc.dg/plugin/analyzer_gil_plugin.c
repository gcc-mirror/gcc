/* Proof-of-concept of a -fanalyzer plugin.
   Detect (some) uses of CPython API outside of the Global Interpreter Lock.
   https://docs.python.org/3/c-api/init.html#thread-state-and-the-global-interpreter-lock
*/
/* { dg-options "-g" } */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "json.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"

int plugin_is_GPL_compatible;

#if ENABLE_ANALYZER

namespace ana {

static bool
type_based_on_pyobject_p (tree type)
{
  /* Ideally we'd also check for "subclasses" here by iterating up the
     first field of each struct.  */
  if (TREE_CODE (type) != RECORD_TYPE)
    return false;
  tree name = TYPE_IDENTIFIER (type);
  if (!name)
    return false;
  return id_equal (name, "PyObject");
}

/* An experimental state machine, for tracking whether the GIL is held,
   as global state..  */

class gil_state_machine : public state_machine
{
public:
  gil_state_machine (logger *logger);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const FINAL OVERRIDE;

  bool can_purge_p (state_t s) const FINAL OVERRIDE;

  void check_for_pyobject_usage_without_gil (sm_context *sm_ctxt,
					     const supernode *node,
					     const gimple *stmt,
					     tree op) const;

 private:
  void check_for_pyobject_in_call (sm_context *sm_ctxt,
				   const supernode *node,
				   const gcall *call,
				   tree callee_fndecl) const;

 public:
  /* These states are "global", rather than per-expression.  */

  /* State for when we've released the GIL.  */
  state_t m_released_gil;

  /* Stop state.  */
  state_t m_stop;
};

/* Subclass for diagnostics involving the GIL.  */

class gil_diagnostic : public pending_diagnostic
{
public:
  location_t fixup_location (location_t loc) const FINAL OVERRIDE
  {
    /* Ideally we'd check for specific macros here, and only
       resolve certain macros.  */
    if (linemap_location_from_macro_expansion_p (line_table, loc))
      loc = linemap_resolve_location (line_table, loc,
				      LRK_MACRO_EXPANSION_POINT, NULL);
    return loc;
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.is_global_p ()
	&& change.m_new_state == m_sm.m_released_gil)
      return change.formatted_print ("releasing the GIL here");
    if (change.is_global_p ()
	&& change.m_new_state == m_sm.get_start_state ())
      return change.formatted_print ("acquiring the GIL here");
    return label_text ();
  }

 protected:
  gil_diagnostic (const gil_state_machine &sm) : m_sm (sm)
  {
  }

 private:
  const gil_state_machine &m_sm;
};

class double_save_thread : public gil_diagnostic
{
 public:
  double_save_thread (const gil_state_machine &sm, const gcall *call)
  : gil_diagnostic (sm), m_call (call)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "double_save_thread";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other) const OVERRIDE
  {
    const double_save_thread &sub_other
      = (const double_save_thread &)base_other;
    return m_call == sub_other.m_call;
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, 0,
		       "nested usage of %qs", "Py_BEGIN_ALLOW_THREADS");
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("nested usage of %qs here",
			       "Py_BEGIN_ALLOW_THREADS");
  }

 private:
  const gcall *m_call;
};

class fncall_without_gil : public gil_diagnostic
{
 public:
  fncall_without_gil (const gil_state_machine &sm, const gcall *call,
		      tree callee_fndecl, unsigned arg_idx)
  : gil_diagnostic (sm), m_call (call), m_callee_fndecl (callee_fndecl),
    m_arg_idx (arg_idx)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "fncall_without_gil";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other) const OVERRIDE
  {
    const fncall_without_gil &sub_other
      = (const fncall_without_gil &)base_other;
    return (m_call == sub_other.m_call
	    && m_callee_fndecl == sub_other.m_callee_fndecl
	    && m_arg_idx == sub_other.m_arg_idx);
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    /* There isn't a warning ID for use to use.  */
    if (m_callee_fndecl)
      return warning_at (rich_loc, 0,
			 "use of PyObject as argument %i of %qE"
			 " without the GIL",
			 m_arg_idx + 1, m_callee_fndecl);
    else
      return warning_at (rich_loc, 0,
			 "use of PyObject as argument %i of call"
			 " without the GIL",
			 m_arg_idx + 1, m_callee_fndecl);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_callee_fndecl)
      return ev.formatted_print ("use of PyObject as argument %i of %qE here"
				 " without the GIL",
				 m_arg_idx + 1, m_callee_fndecl);
    else
      return ev.formatted_print ("use of PyObject as argument %i of call here"
				 " without the GIL",
				 m_arg_idx + 1, m_callee_fndecl);
  }

 private:
  const gcall *m_call;
  tree m_callee_fndecl;
  unsigned m_arg_idx;
};

class pyobject_usage_without_gil : public gil_diagnostic
{
 public:
  pyobject_usage_without_gil (const gil_state_machine &sm, tree expr)
  : gil_diagnostic (sm), m_expr (expr)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "pyobject_usage_without_gil";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other) const OVERRIDE
  {
    return same_tree_p (m_expr,
			((const pyobject_usage_without_gil&)base_other).m_expr);
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    /* There isn't a warning ID for use to use.  */
    return warning_at (rich_loc, 0,
		       "use of PyObject %qE without the GIL", m_expr);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("PyObject %qE used here without the GIL",
			       m_expr);
  }

 private:
  tree m_expr;
};

/* gil_state_machine's ctor.  */

gil_state_machine::gil_state_machine (logger *logger)
: state_machine ("gil", logger)
{
  m_released_gil = add_state ("released_gil");
  m_stop = add_state ("stop");
}

struct cb_data
{
  cb_data (const gil_state_machine &sm, sm_context *sm_ctxt,
	   const supernode *snode, const gimple *stmt)
  : m_sm (sm), m_sm_ctxt (sm_ctxt), m_snode (snode), m_stmt (stmt)
  {
  }

  const gil_state_machine &m_sm;
  sm_context *m_sm_ctxt;
  const supernode *m_snode;
  const gimple *m_stmt;
};

static bool
check_for_pyobject (gimple *, tree op, tree, void *data)
{
  cb_data *d = (cb_data *)data;
  d->m_sm.check_for_pyobject_usage_without_gil (d->m_sm_ctxt, d->m_snode,
						d->m_stmt, op);
  return true;
}

/* Assuming that the GIL has been released, complain about any
   PyObject * arguments passed to CALL.  */

void
gil_state_machine::check_for_pyobject_in_call (sm_context *sm_ctxt,
					       const supernode *node,
					       const gcall *call,
					       tree callee_fndecl) const
{
  for (unsigned i = 0; i < gimple_call_num_args (call); i++)
    {
      tree arg = gimple_call_arg (call, i);
      if (TREE_CODE (TREE_TYPE (arg)) != POINTER_TYPE)
	continue;
      tree type = TREE_TYPE (TREE_TYPE (arg));
      if (type_based_on_pyobject_p (type))
	{
	  sm_ctxt->warn (node, call, NULL_TREE,
			 new fncall_without_gil (*this, call,
						 callee_fndecl,
						 i));
	  sm_ctxt->set_global_state (m_stop);
	}
    }
}

/* Implementation of state_machine::on_stmt vfunc for gil_state_machine.  */

bool
gil_state_machine::on_stmt (sm_context *sm_ctxt,
			    const supernode *node,
			    const gimple *stmt) const
{
  const state_t global_state = sm_ctxt->get_global_state ();
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
	{
	  if (is_named_call_p (callee_fndecl, "PyEval_SaveThread", call, 0))
	    {
	      if (0)
		inform (input_location, "found call to %qs",
			"PyEval_SaveThread");
	      if (global_state == m_released_gil)
		{
		  sm_ctxt->warn (node, stmt, NULL_TREE,
				 new double_save_thread (*this, call));
		  sm_ctxt->set_global_state (m_stop);
		}
	      else
		sm_ctxt->set_global_state (m_released_gil);
	      return true;
	    }
	  else if (is_named_call_p (callee_fndecl, "PyEval_RestoreThread",
				    call, 1))
	    {
	      if (0)
		inform (input_location, "found call to %qs",
			"PyEval_SaveThread");
	      if (global_state == m_released_gil)
		sm_ctxt->set_global_state (m_start);
	      return true;
	    }
	  else if (global_state == m_released_gil)
	    {
	      /* Find PyObject * args of calls to fns with unknown bodies.  */
	      if (!fndecl_has_gimple_body_p (callee_fndecl))
		check_for_pyobject_in_call (sm_ctxt, node, call, callee_fndecl);
	    }
	}
      else if (global_state == m_released_gil)
	check_for_pyobject_in_call (sm_ctxt, node, call, NULL);
    }
  else
    if (global_state == m_released_gil)
      {
	/* Walk the stmt, finding uses of PyObject (or "subclasses").  */
	cb_data d (*this, sm_ctxt, node, stmt);
	walk_stmt_load_store_addr_ops (const_cast <gimple *> (stmt), &d,
				       check_for_pyobject,
				       check_for_pyobject,
				       check_for_pyobject);
    }
  return false;
}

bool
gil_state_machine::can_purge_p (state_t s ATTRIBUTE_UNUSED) const
{
  return true;
}

void
gil_state_machine::check_for_pyobject_usage_without_gil (sm_context *sm_ctxt,
							 const supernode *node,
							 const gimple *stmt,
							 tree op) const
{
  tree type = TREE_TYPE (op);
  if (type_based_on_pyobject_p (type))
    {
      sm_ctxt->warn (node, stmt, NULL_TREE,
		     new pyobject_usage_without_gil (*this, op));
      sm_ctxt->set_global_state (m_stop);
    }
}

/* Callback handler for the PLUGIN_ANALYZER_INIT event.  */

static void
gil_analyzer_init_cb (void *gcc_data, void */*user_data*/)
{
  ana::plugin_analyzer_init_iface *iface
    = (ana::plugin_analyzer_init_iface *)gcc_data;
  LOG_SCOPE (iface->get_logger ());
  if (0)
    inform (input_location, "got here: gil_analyzer_init_cb");
  iface->register_state_machine (new gil_state_machine (iface->get_logger ()));
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
#if ENABLE_ANALYZER
  const char *plugin_name = plugin_info->base_name;
  if (0)
    inform (input_location, "got here; %qs", plugin_name);
  register_callback (plugin_info->base_name,
		     PLUGIN_ANALYZER_INIT,
		     ana::gil_analyzer_init_cb,
		     NULL); /* void *user_data */
#else
  sorry_no_analyzer ();
#endif
  return 0;
}
