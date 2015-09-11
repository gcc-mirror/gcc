/* A plugin example that shows which function definitions are caught by PLUGIN_START_FUNCTION and PLUGIN_FINISH_FUNCTION */

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

/* Callback function to invoke when GCC starts a function definition*/

void plugin_start_parse_function (void *event_data, void *data)
{
  tree fndef = (tree) event_data;
  warning (0, G_("Start fndef %s"),
           IDENTIFIER_POINTER (DECL_NAME (fndef)));
}

/* Callback function to invoke after GCC finishes a function definition. */

void plugin_finish_parse_function (void *event_data, void *data)
{
  tree fndef = (tree) event_data;
  warning (0, G_("Finish fndef %s"),
           IDENTIFIER_POINTER (DECL_NAME (fndef)));
}

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;

  register_callback (plugin_name, PLUGIN_START_PARSE_FUNCTION,
                     plugin_start_parse_function, NULL);

  register_callback (plugin_name, PLUGIN_FINISH_PARSE_FUNCTION,
                     plugin_finish_parse_function, NULL);
  return 0;
}
