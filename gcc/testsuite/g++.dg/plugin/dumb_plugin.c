/* A trivial (dumb) plugin example that shows how to use the GCC plugin
   mechanism.  */

#include "gcc-plugin.h"
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "toplev.h"
#include "diagnostic.h"
#include "context.h"

int plugin_is_GPL_compatible;

/* Callback function to invoke after GCC finishes parsing a struct.  */

void
handle_struct (void *event_data, void *data)
{
  tree type = (tree) event_data;
  warning (0, G_("Process struct %s"),
           IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
}

/* Callback function to invoke before the function body is genericized.  */ 

void
handle_pre_generic (void *event_data, void *data)
{
  tree fndecl = (tree) event_data;
  warning (0, G_("Before genericizing function %s"),
           IDENTIFIER_POINTER (DECL_NAME (fndecl)));
}

/* Callback function to invoke after GCC finishes the compilation unit.  */

void
handle_end_of_compilation_unit (void *event_data, void *data)
{
  warning (0, G_("End of compilation unit"));
}


static unsigned int
execute_dumb_plugin_example (void)
{
  warning (0, G_("Analyze function %s"),
           IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
  return 0;
}

static bool
gate_dumb_plugin_example (void)
{
  return true;
}

static struct gimple_opt_pass pass_dumb_plugin_example =
{
  {
    GIMPLE_PASS,
    "dumb_plugin_example",                /* name */
    OPTGROUP_NONE,                        /* optinfo_flags */
    gate_dumb_plugin_example,             /* gate */
    execute_dumb_plugin_example,          /* execute */
    NULL,                                 /* sub */
    NULL,                                 /* next */
    0,                                    /* static_pass_number */
    TV_NONE,                              /* tv_id */
    PROP_cfg,                             /* properties_required */
    0,                                    /* properties_provided */
    0,                                    /* properties_destroyed */
    0,                                    /* todo_flags_start */
    0					  /* todo_flags_finish */
  }
};

/* Initialization function that GCC calls. This plugin takes an argument
   that specifies the name of the reference pass and an instance number,
   both of which determine where the plugin pass should be inserted.  */

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;
  char *ref_pass_name = NULL;
  int ref_instance_number = 0;
  int i;

  /* Process the plugin arguments. This plugin takes the following arguments:
     ref-pass-name=<PASS_NAME> and ref-pass-instance-num=<NUM>.  */
  for (i = 0; i < argc; ++i)
    {
      if (!strcmp (argv[i].key, "ref-pass-name"))
        {
          if (argv[i].value)
            ref_pass_name = argv[i].value;
          else
            warning (0, G_("option '-fplugin-arg-%s-ref-pass-name'"
                           " requires a pass name"), plugin_name);
        }
      else if (!strcmp (argv[i].key, "ref-pass-instance-num"))
        {
          if (argv[i].value)
            ref_instance_number = strtol (argv[i].value, NULL, 0);
          else
            warning (0, G_("option '-fplugin-arg-%s-ref-pass-instance-num'"
                           " requires an integer value"), plugin_name);
        }
      else
        warning (0, G_("plugin %qs: unrecognized argument %qs ignored"),
                 plugin_name, argv[i].key);
    }

  if (!ref_pass_name)
    {
      error (G_("plugin %qs requires a reference pass name"), plugin_name);
      return 1;
    }

  pass_info.pass = make_pass_dumb_plugin_example (g);
  pass_info.reference_pass_name = ref_pass_name;
  pass_info.ref_pass_instance_number = ref_instance_number;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;

  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

  register_callback (plugin_name, PLUGIN_FINISH_TYPE, handle_struct, NULL);

  register_callback (plugin_name, PLUGIN_PRE_GENERICIZE,
                     handle_pre_generic, NULL);

  register_callback (plugin_name, PLUGIN_FINISH_UNIT,
                     handle_end_of_compilation_unit, NULL);
  return 0;
}
