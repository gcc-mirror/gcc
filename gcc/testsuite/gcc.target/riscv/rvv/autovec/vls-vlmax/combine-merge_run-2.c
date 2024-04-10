/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "combine-merge-2.c"

int
main ()
{
  int64_t a = -1789089;
  int64_t b = 8916156;

  int64_t v_vnx16di[sizeof (vnx16di) / sizeof (int64_t)];
  f_vnx16di (a, b, v_vnx16di);

  for (int i = 0; i < sizeof (vnx16di) / sizeof (int64_t); i++)
    {
      if (i < 5)
	{
	  if (v_vnx16di[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16di[i] != b)
	    __builtin_abort ();
	}
    }

  return 0;
}
