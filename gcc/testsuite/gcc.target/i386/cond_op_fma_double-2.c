/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#include "cond_op_fma_double-1.c"
#define FMA3_O2(OPNAME, OP1, OP2)				\
  void								\
  __attribute__ ((noipa,optimize ("O2")))			\
  foo3_o2_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      {								\
	TYPE tmp = MAX(d[i], e[i]);				\
	if (b[i] < c[i])					\
	  j[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 tmp);	\
	else							\
	  j[i] = tmp;						\
      }								\
  }

#define FMAZ_O2(OPNAME, OP1, OP2)				\
  void								\
  __attribute__ ((noipa,optimize ("O2")))			\
  fooz_o2_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      if (b[i] < c[i])						\
	j[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 a[i]);	\
      else							\
	j[i] = .0;						\
  }

#define FMA1_O2(OPNAME, OP1, OP2)				\
  void								\
  __attribute__ ((noipa,optimize ("O2")))			\
  foo1_o2_##OPNAME ()						\
  {								\
    for (int i = 0; i != NUM; i++)				\
      if (b[i] < c[i])						\
	j[i] = __BUILTIN_FMA (OP1 d[i], e[i], OP2 a[i]);	\
      else							\
	j[i] = d[i];						\
  }

FMAZ_O2 (fma,, +);
FMAZ_O2 (fms,, -);
FMAZ_O2 (fnma, -, +);
FMAZ_O2 (fnms, -, -);

FMA1_O2 (fma,, +);
FMA1_O2 (fms,, -);
FMA1_O2 (fnma, -, +);
FMA1_O2 (fnms, -, -);

FMA3_O2 (fma,, +);
FMA3_O2 (fms,, -);
FMA3_O2 (fnma, -, +);
FMA3_O2 (fnms, -, -);

static void
test_256 (void)
{
  int sign = -1;
  for (int i = 0; i != NUM; i++)
    {
      a[i] = 0;
      d[i] = i * 2;
      e[i] = i * i * 3 - i * 9 + 153;
      b[i] = i * 83;
      c[i] = b[i] + sign;
      sign *= -1;
      j[i] = 1;
    }
  foo1_o2_fma ();
  /* foo1_fma need to be after foo1_o2_fma since
     it changes a[i] which is used by foo1_o2_fma.  */
  foo1_fma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
	abort ();
      a[i] = 0;
      b[i] = 1;
    }

  foo1_o2_fms ();
  foo1_fms ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo1_o2_fnma ();
  foo1_fnma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo1_o2_fnms ();
  foo1_fnms ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  fooz_o2_fma ();
  fooz_fma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      b[i] = 1;
    }

  fooz_o2_fms ();
  fooz_fms ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  fooz_o2_fnma ();
  fooz_fnma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  fooz_o2_fnms ();
  fooz_fnms ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo3_o2_fma ();
  foo3_fma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      b[i] = 1;
    }

  foo3_o2_fms ();
  foo3_fms ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo3_o2_fnma ();
  foo3_fnma ();
  for (int i = 0; i != NUM; i++)
    {
      if (a[i] != j[i])
  	abort ();
      a[i] = 0;
      j[i] = 1;
    }

  foo3_o2_fnms ();
  foo3_fnms ();
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
