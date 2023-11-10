/* { dg-do run { target { riscv_v } } } */
/* { dg-options "--param riscv-autovec-preference=fixed-vlmax -O3" } */

#include "init-same-tail-5.c"

int
main ()
{
  int64_t a = -178908923423;
  int64_t b = -891615645644;
  int64_t c = 78908923423;
  int64_t d = 81615645644;
  int64_t v_vnx16di[sizeof (vnx16di) / sizeof (int64_t)];

  f_vnx16di (a, b, c, d, v_vnx16di);

  for (int i = 0; i < sizeof (vnx16di) / sizeof (int64_t); i++)
    {
      if (i == 0)
	{
	  if (v_vnx16di[i] != a)
	    __builtin_abort ();
	}
      else if (i == 1)
	{
	  if (v_vnx16di[i] != b)
	    __builtin_abort ();
	}
      else if (i == 2)
	{
	  if (v_vnx16di[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16di[i] != d)
	    __builtin_abort ();
	}
    }

  return 0;
}
