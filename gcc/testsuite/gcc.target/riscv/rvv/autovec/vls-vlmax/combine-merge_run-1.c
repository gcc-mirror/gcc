/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "combine-merge-1.c"

int
main ()
{
  double a = -1789089.23423;
  double b = -8916156.45644;

  double v_vnx16df[sizeof (vnx16df) / sizeof (double)];
  f_vnx16df (a, b, v_vnx16df);

  for (int i = 0; i < sizeof (vnx16df) / sizeof (double); i++)
    {
      if (i < 5)
	{
	  if (v_vnx16df[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16df[i] != b)
	    __builtin_abort ();
	}
    }

  return 0;
}
