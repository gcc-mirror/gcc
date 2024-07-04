/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "insert-1.c"

int
main ()
{
  int64_t in[16] = {-458615, -1551,  -552, -458615, -45815, -551,  -2,	  -4585,
		    -134,    -99294, -789, -51284,  -7324,  -3333, -6666, -11};

  int64_t v_vnx2di[sizeof (vnx2di) / sizeof (int64_t)];
  f_vnx2di (in[0], in[1], v_vnx2di);
  for (int i = 0; i < sizeof (vnx2di) / sizeof (int64_t); i++)
    {
      if (v_vnx2di[i] != in[i])
	__builtin_abort ();
    }

  int64_t v_vnx4di[sizeof (vnx4di) / sizeof (int64_t)];
  f_vnx4di (in[0], in[1], in[2], in[3], v_vnx4di);
  for (int i = 0; i < sizeof (vnx2di) / sizeof (int64_t); i++)
    {
      if (v_vnx2di[i] != in[i])
	__builtin_abort ();
    }

  int64_t v_vnx8di[sizeof (vnx8di) / sizeof (int64_t)];
  f_vnx8di (in[0], in[1], in[2], in[3], in[4], in[5], in[6], in[7], v_vnx8di);
  for (int i = 0; i < sizeof (vnx2di) / sizeof (int64_t); i++)
    {
      if (v_vnx2di[i] != in[i])
	__builtin_abort ();
    }

  int64_t v_vnx16di[sizeof (vnx16di) / sizeof (int64_t)];
  f_vnx16di (in[0], in[1], in[2], in[3], in[4], in[5], in[6], in[7], in[8],
	     in[9], in[10], in[11], in[12], in[13], in[14], in[15], v_vnx16di);
  for (int i = 0; i < sizeof (vnx2di) / sizeof (int64_t); i++)
    {
      if (v_vnx2di[i] != in[i])
	__builtin_abort ();
    }

  return 0;
}
