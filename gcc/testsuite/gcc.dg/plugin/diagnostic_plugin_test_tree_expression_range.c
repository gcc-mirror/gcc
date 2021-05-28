/* This plugin verifies the source-code location ranges of
   expressions, at the pre-gimplification tree stage.  */
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
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"
#include "print-tree.h"

int plugin_is_GPL_compatible;

static void
emit_warning (location_t loc)
{
  source_range src_range = get_range_from_loc (line_table, loc);
  warning_at (loc, 0,
	      "tree range %i:%i-%i:%i",
	      LOCATION_LINE (src_range.m_start),
	      LOCATION_COLUMN (src_range.m_start),
	      LOCATION_LINE (src_range.m_finish),
	      LOCATION_COLUMN (src_range.m_finish));
}

tree
cb_walk_tree_fn (tree * tp, int * walk_subtrees,
		 void * data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*tp) != CALL_EXPR)
    return NULL_TREE;

  tree call_expr = *tp;
  tree fn = CALL_EXPR_FN (call_expr);
  if (TREE_CODE (fn) != ADDR_EXPR)
    return NULL_TREE;
  fn = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fn) != FUNCTION_DECL)
    return NULL_TREE;
  if (strcmp (IDENTIFIER_POINTER (DECL_NAME (fn)), "__emit_expression_range"))
    return NULL_TREE;

  /* Get arg 1; print it! */
  tree arg = CALL_EXPR_ARG (call_expr, 1);

  emit_warning (EXPR_LOCATION (arg));

  return NULL_TREE;
}

static void
callback (void *gcc_data, void *user_data)
{
  tree fndecl = (tree)gcc_data;
  walk_tree (&DECL_SAVED_TREE (fndecl), cb_walk_tree_fn, NULL, NULL);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  global_dc->caret_max_width = 130;

  register_callback (plugin_name,
		     PLUGIN_PRE_GENERICIZE,
		     callback,
		     NULL);

  return 0;
}
