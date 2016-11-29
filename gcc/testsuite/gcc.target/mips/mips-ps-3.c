/* { dg-do run } */
/* { dg-options "-mpaired-single forbid_cpu=octeon.* (REQUIRES_STDLIB)" } */

/* Test MIPS paired-single conditional move */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__((vector_size(8)));

NOMIPS16 v2sf test0 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test1 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test2 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test3 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test4 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test5 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test6 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test7 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test8 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test9 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test10 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test11 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test12 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test13 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test14 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test15 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test16 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test17 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test18 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test19 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test20 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test21 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test22 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test23 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test24 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test25 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test26 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test27 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test28 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test29 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test30 (v2sf a, v2sf b, v2sf c, v2sf d);
NOMIPS16 v2sf test31 (v2sf a, v2sf b, v2sf c, v2sf d);

float qnan = 1.0f/0.0f - 1.0f/0.0f;

NOMIPS16 int main ()
{
  float f1;
  v2sf a, b, c, d, e, f;

  /* Case 1 {diff, diff} */
  /* movt.ps */
  a = (v2sf) {5, 12};
  b = (v2sf) {9, 6};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movt_c_eq_ps (a, b, c, d);
  f = (v2sf) {33, 123};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* movf.ps */
  e = __builtin_mips_movf_c_eq_ps (a, b, c, d);
  f = (v2sf) {8, 78};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* Case 2 {same, diff} */
  /* movt.ps */
  a = (v2sf) {5, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movt_c_eq_ps (a, b, c, d);
  f = (v2sf) {8, 123};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* movf.ps */
  e = __builtin_mips_movf_c_eq_ps (a, b, c, d);
  f = (v2sf) {33, 78};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* Case 3 {diff, same} */
  /* movt.ps */
  a = (v2sf) {5, 12};
  b = (v2sf) {9, 12};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movt_c_eq_ps (a, b, c, d);
  f = (v2sf) {33, 78};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* movf.ps */
  e = __builtin_mips_movf_c_eq_ps (a, b, c, d);
  f = (v2sf) {8, 123};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* Case 4 {same, same} */
  /* movt.ps */
  a = (v2sf) {5, 12};
  b = (v2sf) {5, 12};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movt_c_eq_ps (a, b, c, d);
  f = (v2sf) {8, 78};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* movf.ps */
  e = __builtin_mips_movf_c_eq_ps (a, b, c, d);
  f = (v2sf) {33, 123};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  /* Test all 16 operators */
  a = (v2sf) {123, 123};
  b = (v2sf) {1000, 1000};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = test0 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test1 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test2 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test3 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test4 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test5 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test6 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test7 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test8 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test9 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test10 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test11 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test12 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test13 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test14 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test15 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test16 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test17 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test18 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test19 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test20 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test21 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test22 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test23 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test24 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test25 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test26 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test27 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test28 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test29 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test30 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test31 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  /* Test all 16 operators with (b, a) */
  a = (v2sf) {123, 123};
  b = (v2sf) {1000, 1000};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = test0 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test1 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test2 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test3 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test4 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test5 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test6 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test7 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test8 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test9 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test10 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test11 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test12 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test13 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test14 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test15 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test16 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test17 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test18 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test19 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test20 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test21 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test22 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test23 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test24 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test25 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test26 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test27 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test28 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test29 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test30 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test31 (b, a, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

#ifndef __FAST_MATH__
  /* Test with NaN */
  a = (v2sf) {qnan, qnan};
  b = (v2sf) {1000, 1000};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = test0 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test1 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test2 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test3 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test4 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test5 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test6 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test7 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test8 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test9 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test10 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test11 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test12 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test13 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test14 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test15 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test16 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test17 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test18 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test19 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test20 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test21 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test22 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test23 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test24 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test25 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test26 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test27 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();

  e = test28 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
  e = test29 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();

  e = test30 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, d) || 
      !__builtin_mips_lower_c_eq_ps (e, d))
    abort ();
  e = test31 (a, b, c, d);
  if (!__builtin_mips_upper_c_eq_ps (e, c) || 
      !__builtin_mips_lower_c_eq_ps (e, c))
    abort ();
#endif

  printf ("Test Passes\n");
  exit (0);
}

NOMIPS16 v2sf test0 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_f_ps (a, b, c, d);
}

NOMIPS16 v2sf test1 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_f_ps (a, b, c, d);
}

NOMIPS16 v2sf test2 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_un_ps (a, b, c, d);
}

NOMIPS16 v2sf test3 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_un_ps (a, b, c, d);
}

NOMIPS16 v2sf test4 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_eq_ps (a, b, c, d);
}

NOMIPS16 v2sf test5 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_eq_ps (a, b, c, d);
}

NOMIPS16 v2sf test6 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ueq_ps (a, b, c, d);
}

NOMIPS16 v2sf test7 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ueq_ps (a, b, c, d);
}

NOMIPS16 v2sf test8 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_olt_ps (a, b, c, d);
}

NOMIPS16 v2sf test9 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_olt_ps (a, b, c, d);
}

NOMIPS16 v2sf test10 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ult_ps (a, b, c, d);
}

NOMIPS16 v2sf test11 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ult_ps (a, b, c, d);
}

NOMIPS16 v2sf test12 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ole_ps (a, b, c, d);
}

NOMIPS16 v2sf test13 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ole_ps (a, b, c, d);
}

NOMIPS16 v2sf test14 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ule_ps (a, b, c, d);
}

NOMIPS16 v2sf test15 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ule_ps (a, b, c, d);
}

NOMIPS16 v2sf test16 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_sf_ps (a, b, c, d);
}

NOMIPS16 v2sf test17 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_sf_ps (a, b, c, d);
}

NOMIPS16 v2sf test18 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ngle_ps (a, b, c, d);
}

NOMIPS16 v2sf test19 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ngle_ps (a, b, c, d);
}

NOMIPS16 v2sf test20 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_seq_ps (a, b, c, d);
}

NOMIPS16 v2sf test21 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_seq_ps (a, b, c, d);
}

NOMIPS16 v2sf test22 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ngl_ps (a, b, c, d);
}

NOMIPS16 v2sf test23 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ngl_ps (a, b, c, d);
}

NOMIPS16 v2sf test24 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_lt_ps (a, b, c, d);
}

NOMIPS16 v2sf test25 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_lt_ps (a, b, c, d);
}

NOMIPS16 v2sf test26 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_nge_ps (a, b, c, d);
}

NOMIPS16 v2sf test27 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_nge_ps (a, b, c, d);
}

NOMIPS16 v2sf test28 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_le_ps (a, b, c, d);
}

NOMIPS16 v2sf test29 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_le_ps (a, b, c, d);
}

NOMIPS16 v2sf test30 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movt_c_ngt_ps (a, b, c, d);
}

NOMIPS16 v2sf test31 (v2sf a, v2sf b, v2sf c, v2sf d)
{
  return __builtin_mips_movf_c_ngt_ps (a, b, c, d);
}
