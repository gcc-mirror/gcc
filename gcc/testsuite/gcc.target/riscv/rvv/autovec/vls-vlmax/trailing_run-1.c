/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3 -mrvv-max-lmul=m8" } */

#include "trailing-1.c"

int
main ()
{
  int64_t a = -1789089;
  int64_t b = 8916156;
  int64_t c = -7789;
  int64_t d = 9156;

  int64_t v_v16di[sizeof (v16di) / sizeof (int64_t)];
  f_v16di (a, b, c, d, v_v16di);

  for (int i = 0; i < sizeof (v16di) / sizeof (int64_t); i++)
    {
      if (i == 0)
	{
	  if (v_v16di[i] != a)
	    __builtin_abort ();
	}
      else if (i == 1)
	{
	  if (v_v16di[i] != b)
	    __builtin_abort ();
	}
      else if (i == 2)
	{
	  if (v_v16di[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_v16di[i] != d)
	    __builtin_abort ();
	}
    }

  return 0;
}
