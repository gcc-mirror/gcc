/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-options "-mips64 -O2 -mips3d -mhard-float -mfp64" } */

/* Test MIPS-3D absolute compare (floats) builtin functions */
#include <stdlib.h>
#include <stdio.h>

int test0 (float a, float b);
int test1 (float a, float b);
int test2 (float a, float b);
int test3 (float a, float b);
int test4 (float a, float b);
int test5 (float a, float b);
int test6 (float a, float b);
int test7 (float a, float b);
int test8 (float a, float b);
int test9 (float a, float b);
int test10 (float a, float b);
int test11 (float a, float b);
int test12 (float a, float b);
int test13 (float a, float b);
int test14 (float a, float b);
int test15 (float a, float b);

int main ()
{
  float a, b;
  int i;

  /* cabs.eq.s */
  a = 12;
  b = -56;
  i = 0;
  if (__builtin_mips_cabs_eq_s(a, b) != i)
     abort ();

  /* cabs.eq.s */
  a = 12;
  b = -12;
  i = 1;
  if (__builtin_mips_cabs_eq_s(a, b) != i)
     abort ();

  /* Test all comparisons */
  a = 10.58;
  b = 567.345;

  i = test0 (a, b);
  if (i != 0)
    abort ();
  i = test1 (a, b);
  if (i != 0)
    abort ();
  i = test2 (a, b);
  if (i != 0)
    abort ();
  i = test3 (a, b);
  if (i != 0)
    abort ();
  i = test4 (a, b);
  if (i != 1)
    abort ();
  i = test5 (a, b);
  if (i != 1)
    abort ();
  i = test6 (a, b);
  if (i != 1)
    abort ();
  i = test7 (a, b);
  if (i != 1)
    abort ();
  i = test8 (a, b);
  if (i != 0)
    abort ();
  i = test9 (a, b);
  if (i != 0)
    abort ();
  i = test10 (a, b);
  if (i != 0)
    abort ();
  i = test11 (a, b);
  if (i != 0)
    abort ();
  i = test12 (a, b);
  if (i != 1)
    abort ();
  i = test13 (a, b);
  if (i != 1)
    abort ();
  i = test14 (a, b);
  if (i != 1)
    abort ();
  i = test15 (a, b);
  if (i != 1)
    abort ();

  /* Reversed arguments */
  i = test0 (b, a);
  if (i != 0)
    abort ();
  i = test1 (b, a);
  if (i != 0)
    abort ();
  i = test2 (b, a);
  if (i != 0)
    abort ();
  i = test3 (b, a);
  if (i != 0)
    abort ();
  i = test4 (b, a);
  if (i != 0)
    abort ();
  i = test5 (b, a);
  if (i != 0)
    abort ();
  i = test6 (b, a);
  if (i != 0)
    abort ();
  i = test7 (b, a);
  if (i != 0)
    abort ();
  i = test8 (b, a);
  if (i != 0)
    abort ();
  i = test9 (b, a);
  if (i != 0)
    abort ();
  i = test10 (b, a);
  if (i != 0)
    abort ();
  i = test11 (b, a);
  if (i != 0)
    abort ();
  i = test12 (b, a);
  if (i != 0)
    abort ();
  i = test13 (b, a);
  if (i != 0)
    abort ();
  i = test14 (b, a);
  if (i != 0)
    abort ();
  i = test15 (b, a);
  if (i != 0)
    abort ();

#ifndef __FAST_MATH__
  /* Test all comparisons */
  a = 1.0f/0.0f - 1.0f/0.0f; // QNaN
  b = 567.345;

  i = test0 (a, b);
  if (i != 0)
    abort ();
  i = test1 (a, b);
  if (i != 1)
    abort ();
  i = test2 (a, b);
  if (i != 0)
    abort ();
  i = test3 (a, b);
  if (i != 1)
    abort ();
  i = test4 (a, b);
  if (i != 0)
    abort ();
  i = test5 (a, b);
  if (i != 1)
    abort ();
  i = test6 (a, b);
  if (i != 0)
    abort ();
  i = test7 (a, b);
  if (i != 1)
    abort ();
  i = test8 (a, b);
  if (i != 0)
    abort ();
  i = test9 (a, b);
  if (i != 1)
    abort ();
  i = test10 (a, b);
  if (i != 0)
    abort ();
  i = test11 (a, b);
  if (i != 1)
    abort ();
  i = test12 (a, b);
  if (i != 0)
    abort ();
  i = test13 (a, b);
  if (i != 1)
    abort ();
  i = test14 (a, b);
  if (i != 0)
    abort ();
  i = test15 (a, b);
  if (i != 1)
    abort ();
#endif

  printf ("Test Passes\n");
  exit (0);
}

int test0 (float a, float b)
{
  return __builtin_mips_cabs_f_s (a, b);
}

int test1 (float a, float b)
{
  return __builtin_mips_cabs_un_s (a, b);
}

int test2 (float a, float b)
{
  return __builtin_mips_cabs_eq_s (a, b);
}

int test3 (float a, float b)
{
  return __builtin_mips_cabs_ueq_s (a, b);
}

int test4 (float a, float b)
{
  return __builtin_mips_cabs_olt_s (a, b);
}

int test5 (float a, float b)
{
  return __builtin_mips_cabs_ult_s (a, b);
}

int test6 (float a, float b)
{
  return __builtin_mips_cabs_ole_s (a, b);
}

int test7 (float a, float b)
{
  return __builtin_mips_cabs_ule_s (a, b);
}

int test8 (float a, float b)
{
  return __builtin_mips_cabs_sf_s (a, b);
}

int test9 (float a, float b)
{
  return __builtin_mips_cabs_ngle_s (a, b);
}

int test10 (float a, float b)
{
  return __builtin_mips_cabs_seq_s (a, b);
}

int test11 (float a, float b)
{
  return __builtin_mips_cabs_ngl_s (a, b);
}

int test12 (float a, float b)
{
  return __builtin_mips_cabs_lt_s (a, b);
}

int test13 (float a, float b)
{
  return __builtin_mips_cabs_nge_s (a, b);
}

int test14 (float a, float b)
{
  return __builtin_mips_cabs_le_s (a, b);
}

int test15 (float a, float b)
{
  return __builtin_mips_cabs_ngt_s (a, b);
}
