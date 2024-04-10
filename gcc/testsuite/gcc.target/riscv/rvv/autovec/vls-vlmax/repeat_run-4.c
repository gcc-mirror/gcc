/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "repeat-4.c"

int
main ()
{
  float a = -9523.33;
  float b = 8156.55;

  float v_vnx4sf[sizeof (vnx4sf) / sizeof (float)];
  f_vnx4sf (a, b, v_vnx4sf);
  for (int i = 0; i < sizeof (vnx4sf) / sizeof (float); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx4sf[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx4sf[i] != b)
	    __builtin_abort ();
	}
    }

  float v_vnx8sf[sizeof (vnx8sf) / sizeof (float)];
  f_vnx8sf (a, b, v_vnx8sf);
  for (int i = 0; i < sizeof (vnx8sf) / sizeof (float); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx8sf[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx8sf[i] != b)
	    __builtin_abort ();
	}
    }

  float v_vnx16sf[sizeof (vnx16sf) / sizeof (float)];
  f_vnx16sf (a, b, v_vnx16sf);
  for (int i = 0; i < sizeof (vnx16sf) / sizeof (float); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx16sf[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16sf[i] != b)
	    __builtin_abort ();
	}
    }

  float v_vnx32sf[sizeof (vnx32sf) / sizeof (float)];
  f_vnx32sf (a, b, v_vnx32sf);
  for (int i = 0; i < sizeof (vnx32sf) / sizeof (float); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx32sf[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx32sf[i] != b)
	    __builtin_abort ();
	}
    }

  return 0;
}
