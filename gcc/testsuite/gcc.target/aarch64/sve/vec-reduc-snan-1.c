/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fsignaling-nans -fdump-tree-forwprop1" } */

#include <arm_sve.h>

#define SNAN_F32_BITS 0x7f800001U

float __GIMPLE ()
reduc_max_dup (float x)
{
  svfloat32_t v;
  float res;

  v = svdup_f32 (x);
  res = .REDUC_MAX (v);
  return res;
}

float __GIMPLE ()
reduc_min_dup (float x)
{
  svfloat32_t v;
  float res;

  v = svdup_f32 (x);
  res = .REDUC_MIN (v);
  return res;
}

typedef float v4sf __attribute__ ((vector_size (16)));
typedef unsigned int v4su __attribute__ ((vector_size (16)));

float __GIMPLE ()
reduc_max_cst (void)
{
  v4su u;
  v4sf v;
  float res;

  u = _Literal (v4su) { SNAN_F32_BITS, SNAN_F32_BITS,
			 SNAN_F32_BITS, SNAN_F32_BITS };
  v = __VIEW_CONVERT <v4sf> (u);
  res = .REDUC_MAX (v);
  return res;
}

float __GIMPLE ()
reduc_min_cst (void)
{
  v4su u;
  v4sf v;
  float res;

  u = _Literal (v4su) { SNAN_F32_BITS, SNAN_F32_BITS,
			 SNAN_F32_BITS, SNAN_F32_BITS };
  v = __VIEW_CONVERT <v4sf> (u);
  res = .REDUC_MIN (v);
  return res;
}

/* { dg-final { scan-tree-dump-times {vec_duplicate_expr} 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX} 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MIN} 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX \(.*Nan} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MIN \(.*Nan} 1 "forwprop1" } } */
