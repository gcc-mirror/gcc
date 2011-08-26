/* A plugin example that shows which declarations are caught by FINISH_DECL */

#include "gcc-plugin.h"
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "diagnostic.h"

int plugin_is_GPL_compatible;

/* Callback function to invoke after GCC finishes a declaration. */

void plugin_finish_decl (void *event_data, void *data)
{
  tree decl = (tree) event_data;

  const char *kind = NULL;
  switch (TREE_CODE(decl)) {
  case FUNCTION_DECL:
    kind = "Function"; break;
  case PARM_DECL:
    kind = "Parameter"; break;
  case VAR_DECL:
    if (DECL_FILE_SCOPE_P(decl))
      kind = "Global";
    else
      kind = "Local";
    break;
  case FIELD_DECL:
    kind = "Field"; break;
  default:
    kind = "Unknown";
  }

  warning (0, G_("Decl %s %s"),
           kind, IDENTIFIER_POINTER (DECL_NAME (decl)));
}

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;

  register_callback (plugin_name, PLUGIN_FINISH_DECL,
                     plugin_finish_decl, NULL);
  return 0;
}
