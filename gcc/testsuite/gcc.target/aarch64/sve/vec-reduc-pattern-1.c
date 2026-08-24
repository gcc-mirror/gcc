/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-forwprop1" } */

#include <arm_sve.h>

/* Check reductions for which the encoded elements and lane count determine
   the result.  Keep reductions whose result can vary with the vector length.  */

int __GIMPLE ()
reduc_max_repeat (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (3, 5, 3, 5);
  res = .REDUC_MAX (v);
  return res;
}

int __GIMPLE ()
reduc_min_repeat (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (3, 5, 3, 5);
  res = .REDUC_MIN (v);
  return res;
}

int __GIMPLE ()
reduc_and_repeat (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (3, 5, 3, 5);
  res = .REDUC_AND (v);
  return res;
}

int __GIMPLE ()
reduc_ior_repeat (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (3, 5, 3, 5);
  res = .REDUC_IOR (v);
  return res;
}

int __GIMPLE ()
reduc_max_series (void)
{
  svint32_t v;
  int res;

  v = svindex_s32 (1, 1);
  res = .REDUC_MAX (v);
  return res;
}

int __GIMPLE ()
reduc_plus_repeat (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (3, 5, 3, 5);
  res = .REDUC_PLUS (v);
  return res;
}

int __GIMPLE ()
reduc_xor_unknown (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (1, 2, 4, 8);
  res = .REDUC_XOR (v);
  return res;
}

int __GIMPLE ()
reduc_xor_repeat_even (void)
{
  svint32_t v;
  int res;

  v = svdupq_s32 (1, 2, 1, 2);
  res = .REDUC_XOR (v);
  return res;
}

/* Form non-stepped constants with foreground and background elements.  */

static inline __attribute__ ((always_inline)) svint32_t
initial_one (void)
{
  svint32_t v = { 3 };
  return v;
}

static inline __attribute__ ((always_inline)) svint32_t
initial_four (void)
{
  svint32_t v = { -3, -5, -6, -7 };
  return v;
}

int __GIMPLE ()
reduc_max_initial_repeat (void)
{
  svint32_t v;
  int res;

  v = initial_one ();
  res = .REDUC_MAX (v);
  return res;
}

int __GIMPLE ()
reduc_xor_initial_repeat (void)
{
  svint32_t v;
  int res;

  v = initial_one ();
  res = .REDUC_XOR (v);
  return res;
}

int __GIMPLE ()
reduc_max_incomplete_pattern (void)
{
  svint32_t v;
  int res;

  v = initial_four ();
  res = .REDUC_MAX (v);
  return res;
}

/* { dg-final { scan-tree-dump-times {\.REDUC_} 4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX \(\{ 1, 2, 3, \.\.\. \}\)} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX \(\{ -3, -5, -6, -7, 0, 0, 0, 0, \.\.\. \}\)} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_PLUS \(\{ 3, 5, \.\.\. \}\)} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_XOR \(\{ 1, 2, 4, 8, \.\.\. \}\)} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 5;} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 3;} 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 1;} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 7;} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 0;} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not {return 6;} "forwprop1" } } */
