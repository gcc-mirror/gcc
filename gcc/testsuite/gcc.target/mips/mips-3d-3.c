/* { dg-do run } */
/* { dg-options "-mips3d forbid_cpu=octeon.* (REQUIRES_STDLIB)" } */

/* Test MIPS-3D absolute compare builtin functions */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__ ((vector_size(8)));

NOMIPS16 int test0 (v2sf a, v2sf b);
NOMIPS16 int test1 (v2sf a, v2sf b);
NOMIPS16 int test2 (v2sf a, v2sf b);
NOMIPS16 int test3 (v2sf a, v2sf b);
NOMIPS16 int test4 (v2sf a, v2sf b);
NOMIPS16 int test5 (v2sf a, v2sf b);
NOMIPS16 int test6 (v2sf a, v2sf b);
NOMIPS16 int test7 (v2sf a, v2sf b);
NOMIPS16 int test8 (v2sf a, v2sf b);
NOMIPS16 int test9 (v2sf a, v2sf b);
NOMIPS16 int test10 (v2sf a, v2sf b);
NOMIPS16 int test11 (v2sf a, v2sf b);
NOMIPS16 int test12 (v2sf a, v2sf b);
NOMIPS16 int test13 (v2sf a, v2sf b);
NOMIPS16 int test14 (v2sf a, v2sf b);
NOMIPS16 int test15 (v2sf a, v2sf b);
NOMIPS16 int test16 (v2sf a, v2sf b);
NOMIPS16 int test17 (v2sf a, v2sf b);
NOMIPS16 int test18 (v2sf a, v2sf b);
NOMIPS16 int test19 (v2sf a, v2sf b);
NOMIPS16 int test20 (v2sf a, v2sf b);
NOMIPS16 int test21 (v2sf a, v2sf b);
NOMIPS16 int test22 (v2sf a, v2sf b);
NOMIPS16 int test23 (v2sf a, v2sf b);
NOMIPS16 int test24 (v2sf a, v2sf b);
NOMIPS16 int test25 (v2sf a, v2sf b);
NOMIPS16 int test26 (v2sf a, v2sf b);
NOMIPS16 int test27 (v2sf a, v2sf b);
NOMIPS16 int test28 (v2sf a, v2sf b);
NOMIPS16 int test29 (v2sf a, v2sf b);
NOMIPS16 int test30 (v2sf a, v2sf b);
NOMIPS16 int test31 (v2sf a, v2sf b);
NOMIPS16 int test32 (v2sf a, v2sf b);
NOMIPS16 int test33 (v2sf a, v2sf b);
NOMIPS16 int test34 (v2sf a, v2sf b);
NOMIPS16 int test35 (v2sf a, v2sf b);
NOMIPS16 int test36 (v2sf a, v2sf b);
NOMIPS16 int test37 (v2sf a, v2sf b);
NOMIPS16 int test38 (v2sf a, v2sf b);
NOMIPS16 int test39 (v2sf a, v2sf b);
NOMIPS16 int test40 (v2sf a, v2sf b);
NOMIPS16 int test41 (v2sf a, v2sf b);
NOMIPS16 int test42 (v2sf a, v2sf b);
NOMIPS16 int test43 (v2sf a, v2sf b);
NOMIPS16 int test44 (v2sf a, v2sf b);
NOMIPS16 int test45 (v2sf a, v2sf b);
NOMIPS16 int test46 (v2sf a, v2sf b);
NOMIPS16 int test47 (v2sf a, v2sf b);
NOMIPS16 int test48 (v2sf a, v2sf b);
NOMIPS16 int test49 (v2sf a, v2sf b);
NOMIPS16 int test50 (v2sf a, v2sf b);
NOMIPS16 int test51 (v2sf a, v2sf b);
NOMIPS16 int test52 (v2sf a, v2sf b);
NOMIPS16 int test53 (v2sf a, v2sf b);
NOMIPS16 int test54 (v2sf a, v2sf b);
NOMIPS16 int test55 (v2sf a, v2sf b);
NOMIPS16 int test56 (v2sf a, v2sf b);
NOMIPS16 int test57 (v2sf a, v2sf b);
NOMIPS16 int test58 (v2sf a, v2sf b);
NOMIPS16 int test59 (v2sf a, v2sf b);
NOMIPS16 int test60 (v2sf a, v2sf b);
NOMIPS16 int test61 (v2sf a, v2sf b);
NOMIPS16 int test62 (v2sf a, v2sf b);
NOMIPS16 int test63 (v2sf a, v2sf b);

float qnan = 1.0f/0.0f - 1.0f/0.0f;

NOMIPS16 int main ()
{
  int little_endian;
  v2sf a, b, c, d;
  int i, j, k, l;

  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  little_endian = endianness_test.i[0];

  /* cabs.eq.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {-56, -78};
  i = 0;
  j = 0;
  k = 0;
  l = 0;
  if (__builtin_mips_any_cabs_eq_ps(a, b) != i)
     abort ();
  if (__builtin_mips_upper_cabs_eq_ps(a, b) != j)
     abort ();
  if (__builtin_mips_lower_cabs_eq_ps(a, b) != k)
     abort ();
  if (__builtin_mips_all_cabs_eq_ps(a, b) != l)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {-12, -78};
  i = 1;
  if (little_endian)
    {
      j = 0;
      k = 1;
    }
  else
    {
      j = 1;
      k = 0;
    }
  l = 0;
  if (__builtin_mips_any_cabs_eq_ps(a, b) != i)
     abort ();
  if (__builtin_mips_upper_cabs_eq_ps(a, b) != j)
     abort ();
  if (__builtin_mips_lower_cabs_eq_ps(a, b) != k)
     abort ();
  if (__builtin_mips_all_cabs_eq_ps(a, b) != l)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {-56, -34};
  i = 1;
  if (little_endian)
    {
      j = 1;
      k = 0;
    }
  else
    {
      j = 0;
      k = 1;
    }
  l = 0;
  if (__builtin_mips_any_cabs_eq_ps(a, b) != i)
     abort ();
  if (__builtin_mips_upper_cabs_eq_ps(a, b) != j)
     abort ();
  if (__builtin_mips_lower_cabs_eq_ps(a, b) != k)
     abort ();
  if (__builtin_mips_all_cabs_eq_ps(a, b) != l)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {12, 34};
  b = (v2sf) {-12, -34};
  i = 1;
  j = 1;
  k = 1;
  l = 1;
  if (__builtin_mips_any_cabs_eq_ps(a, b) != i)
     abort ();
  if (__builtin_mips_upper_cabs_eq_ps(a, b) != j)
     abort ();
  if (__builtin_mips_lower_cabs_eq_ps(a, b) != k)
     abort ();
  if (__builtin_mips_all_cabs_eq_ps(a, b) != l)
     abort ();

  /* Test all comparisons */
  if (little_endian)
    {
      a = (v2sf) {1984.0, 10.58};
      b = (v2sf) {-1984.0, -567.345};
    }
  else
    {
      a = (v2sf) {10.58, 1984.0};
      b = (v2sf) {-567.345, -1984.0};
    }

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
  if (i != 0)
    abort ();
  i = test5 (a, b);
  if (i != 0)
    abort ();
  i = test6 (a, b);
  if (i != 0)
    abort ();
  i = test7 (a, b);
  if (i != 0)
    abort ();
  i = test8 (a, b);
  if (i != 1)
    abort ();
  i = test9 (a, b);
  if (i != 0)
    abort ();
  i = test10 (a, b);
  if (i != 1)
    abort ();
  i = test11 (a, b);
  if (i != 0)
    abort ();
  i = test12 (a, b);
  if (i != 1)
    abort ();
  i = test13 (a, b);
  if (i != 0)
    abort ();
  i = test14 (a, b);
  if (i != 1)
    abort ();
  i = test15 (a, b);
  if (i != 0)
    abort ();
  i = test16 (a, b);
  if (i != 1)
    abort ();
  i = test17 (a, b);
  if (i != 1)
    abort ();
  i = test18 (a, b);
  if (i != 0)
    abort ();
  i = test19 (a, b);
  if (i != 0)
    abort ();
  i = test20 (a, b);
  if (i != 1)
    abort ();
  i = test21 (a, b);
  if (i != 1)
    abort ();
  i = test22 (a, b);
  if (i != 0)
    abort ();
  i = test23 (a, b);
  if (i != 0)
    abort ();
  i = test24 (a, b);
  if (i != 1)
    abort ();
  i = test25 (a, b);
  if (i != 1)
    abort ();
  i = test26 (a, b);
  if (i != 1)
    abort ();
  i = test27 (a, b);
  if (i != 1)
    abort ();
  i = test28 (a, b);
  if (i != 1)
    abort ();
  i = test29 (a, b);
  if (i != 1)
    abort ();
  i = test30 (a, b);
  if (i != 1)
    abort ();
  i = test31 (a, b);
  if (i != 1)
    abort ();
  i = test32 (a, b);
  if (i != 0)
    abort ();
  i = test33 (a, b);
  if (i != 0)
    abort ();
  i = test34 (a, b);
  if (i != 0)
    abort ();
  i = test35 (a, b);
  if (i != 0)
    abort ();
  i = test36 (a, b);
  if (i != 0)
    abort ();
  i = test37 (a, b);
  if (i != 0)
    abort ();
  i = test38 (a, b);
  if (i != 0)
    abort ();
  i = test39 (a, b);
  if (i != 0)
    abort ();
  i = test40 (a, b);
  if (i != 1)
    abort ();
  i = test41 (a, b);
  if (i != 0)
    abort ();
  i = test42 (a, b);
  if (i != 1)
    abort ();
  i = test43 (a, b);
  if (i != 0)
    abort ();
  i = test44 (a, b);
  if (i != 1)
    abort ();
  i = test45 (a, b);
  if (i != 0)
    abort ();
  i = test46 (a, b);
  if (i != 1)
    abort ();
  i = test47 (a, b);
  if (i != 0)
    abort ();
  i = test48 (a, b);
  if (i != 1)
    abort ();
  i = test49 (a, b);
  if (i != 1)
    abort ();
  i = test50 (a, b);
  if (i != 0)
    abort ();
  i = test51 (a, b);
  if (i != 0)
    abort ();
  i = test52 (a, b);
  if (i != 1)
    abort ();
  i = test53 (a, b);
  if (i != 1)
    abort ();
  i = test54 (a, b);
  if (i != 0)
    abort ();
  i = test55 (a, b);
  if (i != 0)
    abort ();
  i = test56 (a, b);
  if (i != 1)
    abort ();
  i = test57 (a, b);
  if (i != 1)
    abort ();
  i = test58 (a, b);
  if (i != 1)
    abort ();
  i = test59 (a, b);
  if (i != 1)
    abort ();
  i = test60 (a, b);
  if (i != 1)
    abort ();
  i = test61 (a, b);
  if (i != 1)
    abort ();
  i = test62 (a, b);
  if (i != 1)
    abort ();
  i = test63 (a, b);
  if (i != 1)
    abort ();

  /* Reverse arguments */
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
  if (i != 1)
    abort ();
  i = test9 (b, a);
  if (i != 0)
    abort ();
  i = test10 (b, a);
  if (i != 1)
    abort ();
  i = test11 (b, a);
  if (i != 0)
    abort ();
  i = test12 (b, a);
  if (i != 1)
    abort ();
  i = test13 (b, a);
  if (i != 0)
    abort ();
  i = test14 (b, a);
  if (i != 1)
    abort ();
  i = test15 (b, a);
  if (i != 0)
    abort ();
  i = test16 (b, a);
  if (i != 0)
    abort ();
  i = test17 (b, a);
  if (i != 0)
    abort ();
  i = test18 (b, a);
  if (i != 0)
    abort ();
  i = test19 (b, a);
  if (i != 0)
    abort ();
  i = test20 (b, a);
  if (i != 0)
    abort ();
  i = test21 (b, a);
  if (i != 0)
    abort ();
  i = test22 (b, a);
  if (i != 0)
    abort ();
  i = test23 (b, a);
  if (i != 0)
    abort ();
  i = test24 (b, a);
  if (i != 1)
    abort ();
  i = test25 (b, a);
  if (i != 0)
    abort ();
  i = test26 (b, a);
  if (i != 1)
    abort ();
  i = test27 (b, a);
  if (i != 0)
    abort ();
  i = test28 (b, a);
  if (i != 1)
    abort ();
  i = test29 (b, a);
  if (i != 0)
    abort ();
  i = test30 (b, a);
  if (i != 1)
    abort ();
  i = test31 (b, a);
  if (i != 0)
    abort ();
  i = test32 (b, a);
  if (i != 0)
    abort ();
  i = test33 (b, a);
  if (i != 0)
    abort ();
  i = test34 (b, a);
  if (i != 0)
    abort ();
  i = test35 (b, a);
  if (i != 0)
    abort ();
  i = test36 (b, a);
  if (i != 0)
    abort ();
  i = test37 (b, a);
  if (i != 0)
    abort ();
  i = test38 (b, a);
  if (i != 0)
    abort ();
  i = test39 (b, a);
  if (i != 0)
    abort ();
  i = test40 (b, a);
  if (i != 1)
    abort ();
  i = test41 (b, a);
  if (i != 0)
    abort ();
  i = test42 (b, a);
  if (i != 1)
    abort ();
  i = test43 (b, a);
  if (i != 0)
    abort ();
  i = test44 (b, a);
  if (i != 1)
    abort ();
  i = test45 (b, a);
  if (i != 0)
    abort ();
  i = test46 (b, a);
  if (i != 1)
    abort ();
  i = test47 (b, a);
  if (i != 0)
    abort ();
  i = test48 (b, a);
  if (i != 0)
    abort ();
  i = test49 (b, a);
  if (i != 0)
    abort ();
  i = test50 (b, a);
  if (i != 0)
    abort ();
  i = test51 (b, a);
  if (i != 0)
    abort ();
  i = test52 (b, a);
  if (i != 0)
    abort ();
  i = test53 (b, a);
  if (i != 0)
    abort ();
  i = test54 (b, a);
  if (i != 0)
    abort ();
  i = test55 (b, a);
  if (i != 0)
    abort ();
  i = test56 (b, a);
  if (i != 1)
    abort ();
  i = test57 (b, a);
  if (i != 0)
    abort ();
  i = test58 (b, a);
  if (i != 1)
    abort ();
  i = test59 (b, a);
  if (i != 0)
    abort ();
  i = test60 (b, a);
  if (i != 1)
    abort ();
  i = test61 (b, a);
  if (i != 0)
    abort ();
  i = test62 (b, a);
  if (i != 1)
    abort ();
  i = test63 (b, a);
  if (i != 0)
    abort ();

#ifndef __FAST_MATH__
  /* Test all comparisons */
  if (little_endian)
    {
      a = (v2sf) {qnan, qnan};
      b = (v2sf) {-1984.0, -567.345};
    }
  else
    {
      a = (v2sf) {qnan, qnan};
      b = (v2sf) {-567.345, -1984.0};
    }

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
  i = test16 (a, b);
  if (i != 0)
    abort ();
  i = test17 (a, b);
  if (i != 0)
    abort ();
  i = test18 (a, b);
  if (i != 0)
    abort ();
  i = test19 (a, b);
  if (i != 0)
    abort ();
  i = test20 (a, b);
  if (i != 1)
    abort ();
  i = test21 (a, b);
  if (i != 1)
    abort ();
  i = test22 (a, b);
  if (i != 1)
    abort ();
  i = test23 (a, b);
  if (i != 1)
    abort ();
  i = test24 (a, b);
  if (i != 0)
    abort ();
  i = test25 (a, b);
  if (i != 0)
    abort ();
  i = test26 (a, b);
  if (i != 0)
    abort ();
  i = test27 (a, b);
  if (i != 0)
    abort ();
  i = test28 (a, b);
  if (i != 1)
    abort ();
  i = test29 (a, b);
  if (i != 1)
    abort ();
  i = test30 (a, b);
  if (i != 1)
    abort ();
  i = test31 (a, b);
  if (i != 1)
    abort ();
  i = test32 (a, b);
  if (i != 0)
    abort ();
  i = test33 (a, b);
  if (i != 0)
    abort ();
  i = test34 (a, b);
  if (i != 0)
    abort ();
  i = test35 (a, b);
  if (i != 0)
    abort ();
  i = test36 (a, b);
  if (i != 1)
    abort ();
  i = test37 (a, b);
  if (i != 1)
    abort ();
  i = test38 (a, b);
  if (i != 1)
    abort ();
  i = test39 (a, b);
  if (i != 1)
    abort ();
  i = test40 (a, b);
  if (i != 0)
    abort ();
  i = test41 (a, b);
  if (i != 0)
    abort ();
  i = test42 (a, b);
  if (i != 0)
    abort ();
  i = test43 (a, b);
  if (i != 0)
    abort ();
  i = test44 (a, b);
  if (i != 1)
    abort ();
  i = test45 (a, b);
  if (i != 1)
    abort ();
  i = test46 (a, b);
  if (i != 1)
    abort ();
  i = test47 (a, b);
  if (i != 1)
    abort ();
  i = test48 (a, b);
  if (i != 0)
    abort ();
  i = test49 (a, b);
  if (i != 0)
    abort ();
  i = test50 (a, b);
  if (i != 0)
    abort ();
  i = test51 (a, b);
  if (i != 0)
    abort ();
  i = test52 (a, b);
  if (i != 1)
    abort ();
  i = test53 (a, b);
  if (i != 1)
    abort ();
  i = test54 (a, b);
  if (i != 1)
    abort ();
  i = test55 (a, b);
  if (i != 1)
    abort ();
  i = test56 (a, b);
  if (i != 0)
    abort ();
  i = test57 (a, b);
  if (i != 0)
    abort ();
  i = test58 (a, b);
  if (i != 0)
    abort ();
  i = test59 (a, b);
  if (i != 0)
    abort ();
  i = test60 (a, b);
  if (i != 1)
    abort ();
  i = test61 (a, b);
  if (i != 1)
    abort ();
  i = test62 (a, b);
  if (i != 1)
    abort ();
  i = test63 (a, b);
  if (i != 1)
    abort ();
#endif

  printf ("Test Passes\n");
  exit (0);
}

NOMIPS16 int test0 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_f_ps (a, b);
}

NOMIPS16 int test1 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_f_ps (a, b);
}

NOMIPS16 int test2 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_f_ps (a, b);
}

NOMIPS16 int test3 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_f_ps (a, b);
}

NOMIPS16 int test4 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_un_ps (a, b);
}

NOMIPS16 int test5 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_un_ps (a, b);
}

NOMIPS16 int test6 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_un_ps (a, b);
}

NOMIPS16 int test7 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_un_ps (a, b);
}

NOMIPS16 int test8 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_eq_ps (a, b);
}

NOMIPS16 int test9 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_eq_ps (a, b);
}

NOMIPS16 int test10 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_eq_ps (a, b);
}

NOMIPS16 int test11 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_eq_ps (a, b);
}

NOMIPS16 int test12 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ueq_ps (a, b);
}

NOMIPS16 int test13 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ueq_ps (a, b);
}

NOMIPS16 int test14 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ueq_ps (a, b);
}

NOMIPS16 int test15 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ueq_ps (a, b);
}

NOMIPS16 int test16 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_olt_ps (a, b);
}

NOMIPS16 int test17 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_olt_ps (a, b);
}

NOMIPS16 int test18 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_olt_ps (a, b);
}

NOMIPS16 int test19 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_olt_ps (a, b);
}

NOMIPS16 int test20 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ult_ps (a, b);
}

NOMIPS16 int test21 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ult_ps (a, b);
}

NOMIPS16 int test22 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ult_ps (a, b);
}

NOMIPS16 int test23 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ult_ps (a, b);
}

NOMIPS16 int test24 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ole_ps (a, b);
}

NOMIPS16 int test25 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ole_ps (a, b);
}

NOMIPS16 int test26 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ole_ps (a, b);
}

NOMIPS16 int test27 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ole_ps (a, b);
}

NOMIPS16 int test28 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ule_ps (a, b);
}

NOMIPS16 int test29 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ule_ps (a, b);
}

NOMIPS16 int test30 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ule_ps (a, b);
}

NOMIPS16 int test31 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ule_ps (a, b);
}

NOMIPS16 int test32 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_sf_ps (a, b);
}

NOMIPS16 int test33 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_sf_ps (a, b);
}

NOMIPS16 int test34 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_sf_ps (a, b);
}

NOMIPS16 int test35 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_sf_ps (a, b);
}

NOMIPS16 int test36 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ngle_ps (a, b);
}

NOMIPS16 int test37 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ngle_ps (a, b);
}

NOMIPS16 int test38 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ngle_ps (a, b);
}

NOMIPS16 int test39 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ngle_ps (a, b);
}

NOMIPS16 int test40 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_seq_ps (a, b);
}

NOMIPS16 int test41 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_seq_ps (a, b);
}

NOMIPS16 int test42 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_seq_ps (a, b);
}

NOMIPS16 int test43 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_seq_ps (a, b);
}

NOMIPS16 int test44 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ngl_ps (a, b);
}

NOMIPS16 int test45 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ngl_ps (a, b);
}

NOMIPS16 int test46 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ngl_ps (a, b);
}

NOMIPS16 int test47 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ngl_ps (a, b);
}

NOMIPS16 int test48 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_lt_ps (a, b);
}

NOMIPS16 int test49 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_lt_ps (a, b);
}

NOMIPS16 int test50 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_lt_ps (a, b);
}

NOMIPS16 int test51 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_lt_ps (a, b);
}

NOMIPS16 int test52 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_nge_ps (a, b);
}

NOMIPS16 int test53 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_nge_ps (a, b);
}

NOMIPS16 int test54 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_nge_ps (a, b);
}

NOMIPS16 int test55 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_nge_ps (a, b);
}

NOMIPS16 int test56 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_le_ps (a, b);
}

NOMIPS16 int test57 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_le_ps (a, b);
}

NOMIPS16 int test58 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_le_ps (a, b);
}

NOMIPS16 int test59 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_le_ps (a, b);
}

NOMIPS16 int test60 (v2sf a, v2sf b)
{
  return __builtin_mips_any_cabs_ngt_ps (a, b);
}

NOMIPS16 int test61 (v2sf a, v2sf b)
{
  return __builtin_mips_upper_cabs_ngt_ps (a, b);
}

NOMIPS16 int test62 (v2sf a, v2sf b)
{
  return __builtin_mips_lower_cabs_ngt_ps (a, b);
}

NOMIPS16 int test63 (v2sf a, v2sf b)
{
  return __builtin_mips_all_cabs_ngt_ps (a, b);
}
