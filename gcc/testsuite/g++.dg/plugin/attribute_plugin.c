/* Demonstrates how to add custom attributes */

#include "gcc-plugin.h"
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "toplev.h"
#include "plugin.h"
#include "diagnostic.h"

int plugin_is_GPL_compatible;

/* Attribute handler callback */

static tree
handle_user_attribute (tree *node, tree name, tree args,
			int flags, bool *no_add_attrs)
{
  return NULL_TREE;
}

/* Attribute definition */

static struct attribute_spec user_attr =
  { "user", 1, 1, false,  false, false, handle_user_attribute, false };

/* Plugin callback called during attribute registration */

static void 
register_attributes (void *event_data, void *data) 
{
  warning (0, G_("Callback to register attributes"));
  register_attribute (&user_attr);
}

/* Callback function to invoke before the function body is genericized.  */ 

void
handle_pre_generic (void *event_data, void *data)
{
  tree fndecl = (tree) event_data;
  tree arg;
  for (arg = DECL_ARGUMENTS(fndecl); arg; arg = DECL_CHAIN (arg)) {
      tree attr;
      for (attr = DECL_ATTRIBUTES (arg); attr; attr = TREE_CHAIN (attr)) {
          tree attrname = TREE_PURPOSE (attr);
          tree attrargs = TREE_VALUE (attr);
          warning (0, G_("attribute '%s' on param '%s' of function %s"),
                   IDENTIFIER_POINTER (attrname),
                   IDENTIFIER_POINTER (DECL_NAME (arg)),
                   IDENTIFIER_POINTER (DECL_NAME (fndecl))
                   );
      }
  }
}

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;
  register_callback (plugin_name, PLUGIN_PRE_GENERICIZE,
                     handle_pre_generic, NULL);

  register_callback (plugin_name, PLUGIN_ATTRIBUTES, register_attributes, NULL);
  return 0;
}
