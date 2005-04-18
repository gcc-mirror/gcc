/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-mips-options "-mips64 -O2 -mips3d -mhard-float -mgp64" } */

/* Test MIPS-3D absolute-compare & branch-if-any-four builtin functions */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__ ((vector_size(8)));

int test0 (v2sf a, v2sf b, v2sf c, v2sf d);
int test1 (v2sf a, v2sf b, v2sf c, v2sf d);
int test2 (v2sf a, v2sf b, v2sf c, v2sf d);
int test3 (v2sf a, v2sf b, v2sf c, v2sf d);
int test4 (v2sf a, v2sf b, v2sf c, v2sf d);
int test5 (v2sf a, v2sf b, v2sf c, v2sf d);
int test6 (v2sf a, v2sf b, v2sf c, v2sf d);
int test7 (v2sf a, v2sf b, v2sf c, v2sf d);
int test8 (v2sf a, v2sf b, v2sf c, v2sf d);
int test9 (v2sf a, v2sf b, v2sf c, v2sf d);
int test10 (v2sf a, v2sf b, v2sf c, v2sf d);
int test11 (v2sf a, v2sf b, v2sf c, v2sf d);
int test12 (v2sf a, v2sf b, v2sf c, v2sf d);
int test13 (v2sf a, v2sf b, v2sf c, v2sf d);
int test14 (v2sf a, v2sf b, v2sf c, v2sf d);
int test15 (v2sf a, v2sf b, v2sf c, v2sf d);
int test16 (v2sf a, v2sf b, v2sf c, v2sf d);
int test17 (v2sf a, v2sf b, v2sf c, v2sf d);
int test18 (v2sf a, v2sf b, v2sf c, v2sf d);
int test19 (v2sf a, v2sf b, v2sf c, v2sf d);
int test20 (v2sf a, v2sf b, v2sf c, v2sf d);
int test21 (v2sf a, v2sf b, v2sf c, v2sf d);
int test22 (v2sf a, v2sf b, v2sf c, v2sf d);
int test23 (v2sf a, v2sf b, v2sf c, v2sf d);
int test24 (v2sf a, v2sf b, v2sf c, v2sf d);
int test25 (v2sf a, v2sf b, v2sf c, v2sf d);
int test26 (v2sf a, v2sf b, v2sf c, v2sf d);
int test27 (v2sf a, v2sf b, v2sf c, v2sf d);
int test28 (v2sf a, v2sf b, v2sf c, v2sf d);
int test29 (v2sf a, v2sf b, v2sf c, v2sf d);
int test30 (v2sf a, v2sf b, v2sf c, v2sf d);
int test31 (v2sf a, v2sf b, v2sf c, v2sf d);

float qnan = 1.0f/0.0f - 1.0f/0.0f;

int main ()
{
  v2sf a, b, c, d;
  int i, j;

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-33, -44};
  c = (v2sf) {55, 66};
  d = (v2sf) {-77, -88};
  i = 0;
  j = 0;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-11, -44};
  c = (v2sf) {55, 66};
  d = (v2sf) {-77, -88};
  i = 1;
  j = 0;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-33, -22};
  c = (v2sf) {55, 66};
  d = (v2sf) {-77, -88};
  i = 1;
  j = 0;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-33, -44};
  c = (v2sf) {55, 66};
  d = (v2sf) {-55, -88};
  i = 1;
  j = 0;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-33, -44};
  c = (v2sf) {55, 66};
  d = (v2sf) {-77, -66};
  i = 1;
  j = 0;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* cabs.eq.ps */
  a = (v2sf) {11, 22};
  b = (v2sf) {-11, -22};
  c = (v2sf) {55, 66};
  d = (v2sf) {-55, -66};
  i = 1;
  j = 1;
  if (__builtin_mips_any_cabs_eq_4s(a, b, c, d) != i)
     abort ();
  if (__builtin_mips_all_cabs_eq_4s(a, b, c, d) != j)
     abort ();

  /* Test all comparisons */
  a = (v2sf) {11, 33};
  b = (v2sf) {-33, -11};
  c = (v2sf) {55, 66};
  d = (v2sf) {-55, -88};

  i = test0 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test1 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test2 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test3 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test4 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test5 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test6 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test7 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test8 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test9 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test10 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test11 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test12 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test13 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test14 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test15 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test16 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test17 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test18 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test19 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test20 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test21 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test22 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test23 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test24 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test25 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test26 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test27 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test28 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test29 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test30 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test31 (a, b, c, d);
  if (i != 0)
    abort ();

  /* Reversed arguments */
  i = test0 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test1 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test2 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test3 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test4 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test5 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test6 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test7 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test8 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test9 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test10 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test11 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test12 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test13 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test14 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test15 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test16 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test17 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test18 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test19 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test20 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test21 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test22 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test23 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test24 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test25 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test26 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test27 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test28 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test29 (b, a, d, c);
  if (i != 0)
    abort ();
  i = test30 (b, a, d, c);
  if (i != 1)
    abort ();
  i = test31 (b, a, d, c);
  if (i != 0)
    abort ();

#ifndef __FAST_MATH__
  /* Test all comparisons */
  a = (v2sf) {qnan, qnan};
  b = (v2sf) {-33, -11};
  c = (v2sf) {qnan, qnan};
  d = (v2sf) {-55, -88};

  i = test0 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test1 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test2 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test3 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test4 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test5 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test6 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test7 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test8 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test9 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test10 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test11 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test12 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test13 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test14 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test15 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test16 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test17 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test18 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test19 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test20 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test21 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test22 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test23 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test24 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test25 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test26 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test27 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test28 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test29 (a, b, c, d);
  if (i != 0)
    abort ();
  i = test30 (a, b, c, d);
  if (i != 1)
    abort ();
  i = test31 (a, b, c, d);
  if (i != 1)
    abort ();
#endif

  printf ("Test Passes\n");
  exit (0);
}

int test0 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_f_4s (a, b, c, d);
}

int test1 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_f_4s (a, b, c, d);
}

int test2 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_un_4s (a, b, c, d);
}

int test3 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_un_4s (a, b, c, d);
}

int test4 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_eq_4s (a, b, c, d);
}

int test5 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_eq_4s (a, b, c, d);
}

int test6 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ueq_4s (a, b, c, d);
}

int test7 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ueq_4s (a, b, c, d);
}

int test8 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_olt_4s (a, b, c, d);
}

int test9 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_olt_4s (a, b, c, d);
}

int test10 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ult_4s (a, b, c, d);
}

int test11 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ult_4s (a, b, c, d);
}

int test12 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ole_4s (a, b, c, d);
}

int test13 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ole_4s (a, b, c, d);
}

int test14 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ule_4s (a, b, c, d);
}

int test15 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ule_4s (a, b, c, d);
}

int test16 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_sf_4s (a, b, c, d);
}

int test17 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_sf_4s (a, b, c, d);
}

int test18 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ngle_4s (a, b, c, d);
}

int test19 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ngle_4s (a, b, c, d);
}

int test20 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_seq_4s (a, b, c, d);
}

int test21 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_seq_4s (a, b, c, d);
}

int test22 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ngl_4s (a, b, c, d);
}

int test23 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ngl_4s (a, b, c, d);
}

int test24 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_lt_4s (a, b, c, d);
}

int test25 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_lt_4s (a, b, c, d);
}

int test26 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_nge_4s (a, b, c, d);
}

int test27 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_nge_4s (a, b, c, d);
}

int test28 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_le_4s (a, b, c, d);
}

int test29 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_le_4s (a, b, c, d);
}

int test30 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_any_cabs_ngt_4s (a, b, c, d);
}

int test31 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_all_cabs_ngt_4s (a, b, c, d);
}
