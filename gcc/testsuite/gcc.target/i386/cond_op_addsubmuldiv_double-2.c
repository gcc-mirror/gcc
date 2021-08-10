/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#include "cond_op_addsubmuldiv_double-1.c"
#define BINO2(OPNAME, OP)			\
  void						\
  __attribute__ ((noipa,optimize ("O2")))	\
  foo_o2_##OPNAME ()				\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	j[i] = d[i] OP e[i];			\
  }


BINO2 (add, +);
BINO2 (sub, -);
BINO2 (mul, *);
BINO2 (div, /);

static void
test_256 (void)
{
  int sign = -1;
  for (int i = 0; i != NUM; i++)
    {
      a[i] = 0.0;
      d[i] = i * 0.5;
      e[i] = i * i * 0.3 - i * 0.9 + 15.3;
      b[i] = i * 0.83;
      c[i] = b[i] + sign;
      sign *= -1;
      j[i] = b[i] < c[i] ? 1.0 : 0.0;
    }
  foo_add ();
  foo_o2_add ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
	abort ();
      a[i] = 0.0;
      j[i] = b[i] < c[i] ? 1.0 : 0.0;
    }

  foo_sub ();
  foo_o2_sub ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0.0;
      j[i] = b[i] < c[i] ? 1.0 : 0.0;
    }

  foo_mul ();
  foo_o2_mul ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
	abort ();
      a[i] = 0.0;
      j[i] = b[i] < c[i] ? 1.0 : 0.0;
    }

  foo_div ();
  foo_o2_div ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
    }
}

static void
test_128 ()
{
  
}
