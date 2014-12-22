#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

int plugin_is_GPL_compatible;

static void
test_double_int_round_udiv (void)
{
  double_int dmin = { 0, HOST_WIDE_INT_MIN };
  double_int dmax = { -1, HOST_WIDE_INT_MAX };
  double_int dnegone = { -1, -1 };
  double_int mod, div;
  div = dmin.udivmod (dnegone, ROUND_DIV_EXPR, &mod);
  if (div.low != 1 || div.high != 0
      || mod.low != 1 || mod.high != HOST_WIDE_INT_MIN)
    abort ();
  div = dmax.udivmod (dnegone, ROUND_DIV_EXPR, &mod);
  if (div.low != 0 || div.high != 0
      || mod.low != dmax.low || mod.high != dmax.high)
    abort ();
}

static void
test_wide_int_round_sdiv (void)
{
  if (wi::ne_p (wi::div_round (2, 3, SIGNED), 1))
    abort ();
  if (wi::ne_p (wi::div_round (1, 3, SIGNED), 0))
    abort ();
  if (wi::ne_p (wi::mod_round (2, 3, SIGNED), -1))
    abort ();
  if (wi::ne_p (wi::mod_round (1, 3, SIGNED), 1))
    abort ();
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  test_double_int_round_udiv ();
  test_wide_int_round_sdiv ();
  return 0;
}
