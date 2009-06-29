/* This plugin tests the correct operation of a PLUGIN_START_UNIT callback.
 * By the time a PLUGIN_START_UNIT callback is invoked, the frontend 
 * initialization should have completed. At least the different *_type_nodes
 * should have been created. This plugin creates an artifical global 
 * interger variable.
 * 
*/
#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "basic-block.h"
#include "gimple.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"

int plugin_is_GPL_compatible;
static tree fake_var = NULL;

static bool
gate_start_unit (void)
{
  return true;
}

static void start_unit_callback (void *gcc_data, void *user_data)
{
  if (integer_type_node) {
    fake_var = build_decl (UNKNOWN_LOCATION, VAR_DECL, 
                           get_identifier ("_fake_var_"),
                           integer_type_node);
    TREE_PUBLIC (fake_var) = 1;
    DECL_ARTIFICIAL (fake_var) = 1;
  }
}

static void finish_unit_callback (void *gcc_data, void *user_data)
{
  if (fake_var == NULL) {
    printf ("fake_var not created \n");
    return;
  }
  if (TREE_CODE (fake_var) != VAR_DECL) {
    printf ("fake_var not a VAR_DECL \n");
    return;
  }
  if (TREE_CODE (TREE_TYPE (fake_var)) != INTEGER_TYPE) {
    printf ("fake_var not INTEGER_TYPE \n");
    return;
  }
  if (DECL_ARTIFICIAL (fake_var) == 0) {
    printf ("fake_var not ARTIFICIAL \n");
    return;
  }
}

int plugin_init (struct plugin_name_args *plugin_info,
                 struct plugin_gcc_version *version)
{
  register_callback ("start_unit", PLUGIN_START_UNIT, &start_unit_callback, NULL);
  register_callback ("finish_unit", PLUGIN_FINISH_UNIT, &finish_unit_callback, NULL);
  return 0;
}
