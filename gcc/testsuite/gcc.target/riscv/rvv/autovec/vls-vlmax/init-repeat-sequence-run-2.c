/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "init-repeat-sequence-3.c"

int
main ()
{
  int64_t a = -178908923423;
  int64_t b = -891615645644;

  int64_t v_vnx8di[sizeof (vnx8di) / sizeof (int64_t)];
  f_vnx8di (a, b, v_vnx8di);
  for (int i = 0; i < sizeof (vnx8di) / sizeof (int64_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx8di[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx8di[i] != b)
	    __builtin_abort ();
	}
    }

  int64_t v_vnx16di[sizeof (vnx16di) / sizeof (int64_t)];
  f_vnx16di (a, b, v_vnx16di);

  for (int i = 0; i < sizeof (vnx16di) / sizeof (int64_t); i++)
    {
      if (i % 2 == 0)
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
