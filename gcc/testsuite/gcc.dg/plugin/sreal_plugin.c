/* Plugin that process internal tests for sreal.  */
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
#include "sreal.h"

int plugin_is_GPL_compatible;

namespace {

static void assert (bool c)
{
  if (!c)
    abort ();
}

const pass_data pass_data_sreal_pass =
{
  GIMPLE_PASS, /* type */
  "sreal", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class sreal_pass : public gimple_opt_pass
{
public:
  sreal_pass(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_sreal_pass, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

private:
  void check_sreal ();

  static void verify_aritmetics (int a, int b);
  static void verify_shifting (int a);
}; // class one_pass

} // anon namespace

void
sreal_pass::verify_aritmetics (int a, int b)
{
  assert (a == -(-(sreal (a))).to_int ());
  assert ((a < b) == (sreal (a) < sreal (b)));
  assert ((a <= b) == (sreal (a) <= sreal (b)));
  assert ((a == b) == (sreal (a) == sreal (b)));
  assert ((a != b) == (sreal (a) != sreal (b)));
  assert ((a > b) == (sreal (a) > sreal (b)));
  assert ((a >= b) == (sreal (a) >= sreal (b)));
  assert ((a + b) == (sreal (a) + sreal (b)).to_int ());
  assert ((a - b) == (sreal (a) - sreal (b)).to_int ());
  assert ((b + a) == (sreal (b) + sreal (a)).to_int ());
  assert ((b - a) == (sreal (b) - sreal (a)).to_int ());
}

void
sreal_pass::verify_shifting (int a)
{
  sreal v = a;

  for (unsigned i = 0; i < 16; i++)
    assert ((a << i) == (v << i).to_int());

  a = a << 16;
  v = v << 16;

  for (unsigned i = 0; i < 16; i++)
    assert ((a >> i) == (v >> i).to_int());
}

void
sreal_pass::check_sreal ()
{
  sreal minimum = INT_MIN;
  sreal maximum = INT_MAX;
  sreal seven = 7;
  sreal minus_two = -2;
  sreal minus_nine = -9;

  assert (minimum.to_int () == INT_MIN);
  assert (maximum.to_int () == INT_MAX);

  assert (!(minus_two < minus_two));
  assert (!(seven < seven));
  assert (seven > minus_two);
  assert (minus_two < seven);
  assert (minus_two != seven);
  assert (minus_two == minus_two);
  assert (seven == seven);

  assert (seven == ((seven << 10) >> 10));

  assert ((seven + minus_two) == 5);
  assert ((seven + minus_nine) == -2);

  for (int a = -100; a < 100; a++)
    for (int b = -100; b < 100; b++)
      {
        verify_aritmetics (a, b);
        verify_aritmetics (INT_MIN + 100, b);
        verify_aritmetics (INT_MAX - 100, b);
      }

  srand (123456);

  for (int i = 0; i < 1000 * 1000; i++)
    {
      verify_aritmetics (rand () % 10, rand () % 1000000);
      verify_aritmetics (rand () % 100, rand () % 10000);
    }

  for (int a = -100; a < 100; a++)
    verify_shifting (a);
}

bool sreal_pass::gate (function *)
{
  return true;
}

unsigned int
sreal_pass::execute (function *)
{
  check_sreal ();

  return 0;
}

int plugin_init (struct plugin_name_args *plugin_info,
                 struct plugin_gcc_version *version)
{
  struct register_pass_info p;

  p.pass = new sreal_pass (g);
  p.reference_pass_name = "cfg";
  p.ref_pass_instance_number = 1;
  p.pos_op = PASS_POS_INSERT_AFTER;

  register_callback ("sreal", PLUGIN_PASS_MANAGER_SETUP, NULL, &p);

  return 0;
}
