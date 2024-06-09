/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "insert-3.c"

int
main ()
{
  double in[16] = {-458.615, -1.551,  -55.2, -4586.15, -4581.5, -55.1,  -2,	  -4.585,
		    -13.4,    -9929.4, -7.89, -512.84,  -73.24,  -33.33, -666.6, -1.1};

  double v_vnx2df[sizeof (vnx2df) / sizeof (double)];
  f_vnx2df (in[0], in[1], v_vnx2df);
  for (int i = 0; i < sizeof (vnx2df) / sizeof (double); i++)
    {
      if (v_vnx2df[i] != in[i])
	__builtin_abort ();
    }

  double v_vnx4df[sizeof (vnx4df) / sizeof (double)];
  f_vnx4df (in[0], in[1], in[2], in[3], v_vnx4df);
  for (int i = 0; i < sizeof (vnx2df) / sizeof (double); i++)
    {
      if (v_vnx2df[i] != in[i])
	__builtin_abort ();
    }

  double v_vnx8df[sizeof (vnx8df) / sizeof (double)];
  f_vnx8df (in[0], in[1], in[2], in[3], in[4], in[5], in[6], in[7], v_vnx8df);
  for (int i = 0; i < sizeof (vnx2df) / sizeof (double); i++)
    {
      if (v_vnx2df[i] != in[i])
	__builtin_abort ();
    }

  double v_vnx16df[sizeof (vnx16df) / sizeof (double)];
  f_vnx16df (in[0], in[1], in[2], in[3], in[4], in[5], in[6], in[7], in[8],
	     in[9], in[10], in[11], in[12], in[13], in[14], in[15], v_vnx16df);
  for (int i = 0; i < sizeof (vnx2df) / sizeof (double); i++)
    {
      if (v_vnx2df[i] != in[i])
	__builtin_abort ();
    }

  return 0;
}
