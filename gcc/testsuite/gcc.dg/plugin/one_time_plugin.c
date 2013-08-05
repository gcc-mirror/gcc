/* Plugin that prints message if it inserted (and invoked) more than once. */
#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "gimple.h"
#include "tree-pass.h"
#include "intl.h"
#include "context.h"

int plugin_is_GPL_compatible;

static bool one_pass_gate (void)
{
  return true;
}

static unsigned int one_pass_exec (void)
{
  static int counter = 0;

  if (counter > 0) {
    printf ("Executed more than once \n");
 }
 counter++;
 return 0;
}

struct gimple_opt_pass one_pass = 
{
  {
  GIMPLE_PASS,
  "cfg",                           /* name */
  OPTGROUP_NONE,                         /* optinfo_flags */
  one_pass_gate,                         /* gate */
  one_pass_exec,       /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  PROP_gimple_any,                      /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0					/* todo_flags_finish */
  }
};


int plugin_init (struct plugin_name_args *plugin_info,
                 struct plugin_gcc_version *version)
{
  struct register_pass_info p;

  p.pass = make_one_pass (g);
  p.reference_pass_name = "cfg";
  p.ref_pass_instance_number = 1;
  p.pos_op = PASS_POS_INSERT_AFTER;

  register_callback ("one_pass", PLUGIN_PASS_MANAGER_SETUP, NULL, &p);

  return 0;
}
