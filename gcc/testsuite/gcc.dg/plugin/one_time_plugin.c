/* Plugin that prints message if it inserted (and invoked) more than once. */
#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tm.h"
#include "toplev.h"
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
#include "tree-pass.h"
#include "intl.h"
#include "context.h"

int plugin_is_GPL_compatible;

namespace {

const pass_data pass_data_one_pass =
{
  GIMPLE_PASS, /* type */
  "cfg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class one_pass : public gimple_opt_pass
{
public:
  one_pass(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_one_pass, ctxt),
      counter(0)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

private:
  int counter;
}; // class one_pass

} // anon namespace

bool one_pass::gate (function *)
{
  return true;
}

unsigned int
one_pass::execute (function *)
{
  if (counter > 0) {
    printf ("Executed more than once \n");
 }
 counter++;
 return 0;
}

gimple_opt_pass *
make_one_pass (gcc::context *ctxt)
{
  return new one_pass (ctxt);
}


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
