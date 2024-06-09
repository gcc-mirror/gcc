/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3 -mrvv-max-lmul=m8" } */

#include "trailing-2.c"

int
main ()
{
  double a = -1789089.4324;
  double b = 8916156.82343;
  double c = -7789.3423;
  double d = 9156.955;

  double v_v16df[sizeof (v16df) / sizeof (double)];
  f_v16df (a, b, c, d, v_v16df);

  for (int i = 0; i < sizeof (v16df) / sizeof (double); i++)
    {
      if (i == 0)
	{
	  if (v_v16df[i] != a)
	    __builtin_abort ();
	}
      else if (i == 1)
	{
	  if (v_v16df[i] != b)
	    __builtin_abort ();
	}
      else if (i == 2)
	{
	  if (v_v16df[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_v16df[i] != d)
	    __builtin_abort ();
	}
    }

  return 0;
}
