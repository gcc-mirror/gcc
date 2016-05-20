/* { dg-options "-O" } */

/* Mark all CALL_EXPRs not within "main" as requiring tail-call. */

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
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"

int plugin_is_GPL_compatible;

tree
cb_walk_tree_fn (tree * tp, int * walk_subtrees,
		 void * data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*tp) != CALL_EXPR)
    return NULL_TREE;

  tree call_expr = *tp;

  /* Forcibly mark the CALL_EXPR as requiring tail-call optimization.  */
  CALL_EXPR_MUST_TAIL_CALL (call_expr) = 1;
  
  return NULL_TREE;
}

static void
callback (void *gcc_data, void *user_data)
{
  tree fndecl = (tree)gcc_data;
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);

  /* Don't mark calls inside "main".  */
  tree decl_name = DECL_NAME (fndecl);
  if (decl_name)
    if (0 == strcmp (IDENTIFIER_POINTER (decl_name), "main"))
      return;

  walk_tree (&DECL_SAVED_TREE (fndecl), cb_walk_tree_fn, NULL, NULL);
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  register_callback (plugin_name,
		     PLUGIN_PRE_GENERICIZE,
		     callback,
		     NULL);

  return 0;
}
