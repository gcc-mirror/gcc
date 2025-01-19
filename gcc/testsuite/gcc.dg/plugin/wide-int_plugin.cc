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
  double_int dmax = { (unsigned HOST_WIDE_INT)-1, HOST_WIDE_INT_MAX };
  double_int dnegone = { (unsigned HOST_WIDE_INT)-1, -1 };
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

static void
test_wide_int_mod_trunc (void)
{
  for (unsigned int i = 1; i < MAX_BITSIZE_MODE_ANY_INT; ++i)
    {
      if (wi::smod_trunc (wi::lshift (1, i + 1) - 3,
			  wi::lshift (1, i) - 1)
	  != wi::lshift (1, i) - 2)
	abort ();
      for (unsigned int base = 32; base <= MAX_BITSIZE_MODE_ANY_INT; base *= 2)
	for (int bias = -1; bias <= 1; ++bias)
	  {
	    unsigned int precision = base + bias;
	    if (i + 1 < precision && precision <= MAX_BITSIZE_MODE_ANY_INT)
	      {
		wide_int one = wi::uhwi (1, precision);
		wide_int a = wi::lshift (one, i + 1) - 3;
		wide_int b = wi::lshift (one, i) - 1;
		wide_int c = wi::lshift (one, i) - 2;
		if (wi::umod_trunc (a, b) != c)
		  abort ();
		if (wi::smod_trunc (a, b) != c)
		  abort ();
		if (wi::smod_trunc (-a, b) != -c)
		  abort ();
		if (wi::smod_trunc (a, -b) != c)
		  abort ();
	      }
	  }
    }
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  test_double_int_round_udiv ();
  test_wide_int_round_sdiv ();
  test_wide_int_mod_trunc ();
  return 0;
}
