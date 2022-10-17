/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#include "cond_op_shift_d-1.c"

#define BINO2C(OPNAME, OP)			\
  void						\
  __attribute__ ((noipa,optimize ("O2")))	\
  foo_o2_##OPNAME##_const ()			\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	j[i] = d[i] OP 3;			\
      else					\
	j[i] = MAX(d[i], e[i]);			\
  }

#define BINO2V(OPNAME, OP)			\
  void						\
  __attribute__ ((noipa,optimize ("O2")))	\
  foo_o2_##OPNAME##_variable ()			\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	j[i] = d[i] OP e[i];			\
      else					\
	j[i] = MAX(d[i], e[i]);			\
  }

BINO2C (shl, <<);
BINO2C (shr, >>);
BINO2V (shl, <<);
BINO2V (shr, >>);

static void
test_256 (void)
{
  int sign = -1;
  for (int i = 0; i != NUM; i++)
    {
      a[i] = 0;
      d[i] = i * 2;
      e[i] = (i * i * 3 - i * 9 + 6)%8;
      b[i] = i * 83;
      c[i] = b[i] + sign;
      sign *= -1;
      j[i] = 1;
    }
  foo_shl_const ();
  foo_o2_shl_const ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
	abort ();
      a[i] = 0;
      b[i] = 1;
    }

  foo_shr_const ();
  foo_o2_shr_const ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo_shl_variable ();
  foo_o2_shl_variable ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
	abort ();
      a[i] = 0;
      b[i] = 1;
    }

  foo_shr_variable ();
  foo_o2_shr_variable ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }
}

static void
test_128 ()
{
  
}
