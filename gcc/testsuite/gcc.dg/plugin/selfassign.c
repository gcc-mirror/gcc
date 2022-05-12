/* This plugin contains an analysis pass that detects and warns about
   self-assignment statements.  */
/* { dg-options "-O" } */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"

int plugin_is_GPL_compatible;

/* Indicate whether to check overloaded operator '=', which is performed by
   default. To disable it, use -fplugin-arg-NAME-no-check-operator-eq.  */
bool check_operator_eq = true;

/* Given a rhs EXPR of a gimple assign statement, if it is
   - SSA_NAME : returns its var decl, or, if it is a temp variable,
                returns the rhs of its SSA def statement.
   - VAR_DECL, PARM_DECL, FIELD_DECL, or a reference expression :
                returns EXPR itself.
   - any other expression : returns NULL_TREE.  */

static tree
get_real_ref_rhs (tree expr)
{
  switch (TREE_CODE (expr))
    {
      case SSA_NAME:
        {
          /* Given a self-assign statement, say foo.x = foo.x,
             the IR (after SSA) looks like:

             D.1797_14 = foo.x;
             foo.x ={v} D.1797_14;

             So if the rhs EXPR is an SSA_NAME of a temp variable,
             e.g. D.1797_14, we need to grab the rhs of its SSA def
             statement (i.e. foo.x).  */
          tree vdecl = SSA_NAME_VAR (expr);
          if ((!vdecl || DECL_ARTIFICIAL (vdecl))
              && !gimple_nop_p (SSA_NAME_DEF_STMT (expr)))
            {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (expr);
              /* We are only interested in an assignment with a single
                 rhs operand because if it is not, the original assignment
                 will not possibly be a self-assignment.  */
              if (gimple_assign_single_p (def_stmt))
                return get_real_ref_rhs (gimple_assign_rhs1 (def_stmt));
              else
                return NULL_TREE;
            }
          else
            return vdecl;
        }
      case VAR_DECL:
      case PARM_DECL:
      case FIELD_DECL:
      case COMPONENT_REF:
      case MEM_REF:
      case ARRAY_REF:
        return expr;
      default:
        return NULL_TREE;
    }
}

/* Given an expression tree, EXPR, that may contains SSA names, returns an
   equivalent tree with the SSA names converted to var/parm/field decls
   so that it can be used with '%E' format modifier when emitting warning
   messages.

   This function currently only supports VAR/PARM/FIELD_DECL, reference
   expressions (COMPONENT_REF, INDIRECT_REF, ARRAY_REF), integer constant,
   and SSA_NAME. If EXPR contains any other tree nodes (e.g. an arithmetic
   expression appears in array index), NULL_TREE is returned.  */

static tree
get_non_ssa_expr (tree expr)
{
  if (!expr)
    return NULL_TREE;
  switch (TREE_CODE (expr))
    {
      case VAR_DECL:
      case PARM_DECL:
      case FIELD_DECL:
        {
          if (DECL_NAME (expr))
            return expr;
          else
            return NULL_TREE;
        }
      case COMPONENT_REF:
        {
          tree base, orig_base = TREE_OPERAND (expr, 0);
          tree component, orig_component = TREE_OPERAND (expr, 1);
          base = get_non_ssa_expr (orig_base);
          if (!base)
            return NULL_TREE;
          component = get_non_ssa_expr (orig_component);
          if (!component)
            return NULL_TREE;
          /* If either BASE or COMPONENT is converted, build a new
             component reference tree.  */
          if (base != orig_base || component != orig_component)
            return build3 (COMPONENT_REF, TREE_TYPE (component),
                           base, component, NULL_TREE);
          else
            return expr;
        }
      case MEM_REF:
        {
          tree orig_base = TREE_OPERAND (expr, 0);
	  if (TREE_CODE (orig_base) == SSA_NAME)
	    {
	      tree base = get_non_ssa_expr (orig_base);
	      if (!base)
		return NULL_TREE;
	      return fold_build2 (MEM_REF, TREE_TYPE (expr),
				  base, TREE_OPERAND (expr, 1));
	    }
	  return expr;
        }
      case ARRAY_REF:
        {
          tree array, orig_array = TREE_OPERAND (expr, 0);
          tree index, orig_index = TREE_OPERAND (expr, 1);
          array = get_non_ssa_expr (orig_array);
          if (!array)
            return NULL_TREE;
          index = get_non_ssa_expr (orig_index);
          if (!index)
            return NULL_TREE;
          /* If either ARRAY or INDEX is converted, build a new array
             reference tree.  */
          if (array != orig_array || index != orig_index)
            return build4 (ARRAY_REF, TREE_TYPE (expr), array, index,
                           TREE_OPERAND (expr, 2), TREE_OPERAND (expr, 3));
          else
            return expr;
        }
      case SSA_NAME:
        {
          tree vdecl = SSA_NAME_VAR (expr);
          if ((!vdecl || DECL_ARTIFICIAL (vdecl))
              && !gimple_nop_p (SSA_NAME_DEF_STMT (expr)))
            {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (expr);
              if (gimple_assign_single_p (def_stmt))
                vdecl = gimple_assign_rhs1 (def_stmt);
            }
          return get_non_ssa_expr (vdecl);
        }
      case INTEGER_CST:
        return expr;
      default:
        /* Return NULL_TREE for any other kind of tree nodes.  */
        return NULL_TREE;
    }
}

/* Given the LHS and (real) RHS of a gimple assign statement, STMT, check if
   they are the same. If so, print a warning message about self-assignment.  */

static void
compare_and_warn (gimple *stmt, tree lhs, tree rhs)
{
  if (operand_equal_p (lhs, rhs, OEP_PURE_SAME))
    {
      location_t location;
      location = (gimple_has_location (stmt)
                  ? gimple_location (stmt)
                  : (DECL_P (lhs)
                     ? DECL_SOURCE_LOCATION (lhs)
                     : input_location));
      /* If LHS contains any tree node not currently supported by
         get_non_ssa_expr, simply emit a generic warning without
         specifying LHS in the message.  */
      lhs = get_non_ssa_expr (lhs);
      if (lhs)
        warning_at (location, 0, G_("%qE is assigned to itself"), lhs);
      else
        warning_at (location, 0, G_("self-assignment detected"));
    }
}

/* Check and warn if STMT is a self-assign statement.  */

static void
warn_self_assign (gimple *stmt)
{
  tree rhs, lhs;

  /* Check assigment statement.  */
  if (gimple_assign_single_p (stmt))
    {
      rhs = get_real_ref_rhs (gimple_assign_rhs1 (stmt));
      if (!rhs)
        return;

      lhs = gimple_assign_lhs (stmt);
      if (TREE_CODE (lhs) == SSA_NAME)
        {
          lhs = SSA_NAME_VAR (lhs);
          if (!lhs || DECL_ARTIFICIAL (lhs))
            return;
        }

      compare_and_warn (stmt, lhs, rhs);
    }
  /* Check overloaded operator '=' (if enabled).  */
  else if (check_operator_eq && is_gimple_call (stmt))
    {
      tree fdecl = gimple_call_fndecl (stmt);
      if (fdecl && (DECL_NAME (fdecl) == maybe_get_identifier ("operator=")))
        {
          /* If 'operator=' takes reference operands, the arguments will be 
             ADDR_EXPR trees. In this case, just remove the address-taken
             operator before we compare the lhs and rhs.  */
          lhs = gimple_call_arg (stmt, 0);
          if (TREE_CODE (lhs) == ADDR_EXPR)
            lhs = TREE_OPERAND (lhs, 0);
          rhs = gimple_call_arg (stmt, 1);
          if (TREE_CODE (rhs) == ADDR_EXPR)
            rhs = TREE_OPERAND (rhs, 0);

          compare_and_warn (stmt, lhs, rhs);
        }
    }
}

namespace {

const pass_data pass_data_warn_self_assign =
{
  GIMPLE_PASS, /* type */
  "warn_self_assign", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_self_assign : public gimple_opt_pass
{
public:
  pass_warn_self_assign(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_warn_self_assign, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_warn_self_assign

unsigned int
pass_warn_self_assign::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        warn_self_assign (gsi_stmt (gsi));
    }

  return 0;
}

} // anon namespace

static gimple_opt_pass *
make_pass_warn_self_assign (gcc::context *ctxt)
{
  return new pass_warn_self_assign (ctxt);
}

/* The initialization routine exposed to and called by GCC. The spec of this
   function is defined in gcc/gcc-plugin.h.

   PLUGIN_NAME - name of the plugin (useful for error reporting)
   ARGC        - the size of the ARGV array
   ARGV        - an array of key-value argument pair

   Returns 0 if initialization finishes successfully.

   Note that this function needs to be named exactly "plugin_init".  */

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;
  bool enabled = true;
  int i;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  /* Self-assign detection should happen after SSA is constructed.  */
  pass_info.pass = make_pass_warn_self_assign (g);
  pass_info.reference_pass_name = "ssa";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;

  /* Process the plugin arguments. This plugin takes the following arguments:
     check-operator-eq, no-check-operator-eq, enable, and disable.
     By default, the analysis is enabled with 'operator=' checked.  */
  for (i = 0; i < argc; ++i)
    {
      if (!strcmp (argv[i].key, "check-operator-eq"))
        {
          if (argv[i].value)
            warning (0, G_("option '-fplugin-arg-%s-check-operator-eq=%s'"
                           " ignored (superfluous '=%s')"),
                     plugin_name, argv[i].value, argv[i].value);
          else
            check_operator_eq = true;
        }
      else if (!strcmp (argv[i].key, "no-check-operator-eq"))
        {
          if (argv[i].value)
            warning (0, G_("option '-fplugin-arg-%s-no-check-operator-eq=%s'"
                           " ignored (superfluous '=%s')"),
                     plugin_name, argv[i].value, argv[i].value);
          else
            check_operator_eq = false;
        }
      else if (!strcmp (argv[i].key, "enable"))
        {
          if (argv[i].value)
            warning (0, G_("option '-fplugin-arg-%s-enable=%s' ignored"
                           " (superfluous '=%s')"),
                     plugin_name, argv[i].value, argv[i].value);
          else
            enabled = true;
        }
      else if (!strcmp (argv[i].key, "disable"))
        {
          if (argv[i].value)
            warning (0, G_("option '-fplugin-arg-%s-disable=%s' ignored"
                           " (superfluous '=%s')"),
                     plugin_name, argv[i].value, argv[i].value);
          else
            enabled = false;
        }
      else
        warning (0, G_("plugin %qs: unrecognized argument %qs ignored"),
                 plugin_name, argv[i].key);
    }

  /* Register this new pass with GCC if the analysis is enabled.  */
  if (enabled)
    register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
                       &pass_info);

  return 0;
}
