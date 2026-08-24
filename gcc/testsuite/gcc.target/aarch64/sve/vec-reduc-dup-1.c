/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-forwprop1" } */

#include <arm_sve.h>

int __GIMPLE ()
reduc_max (int x)
{
  svint32_t v;
  int res;

  v = svdup_s32 (x);
  res = .REDUC_MAX (v);
  return res;
}

int __GIMPLE ()
reduc_min (int x)
{
  svint32_t v;
  int res;

  v = svdup_s32 (x);
  res = .REDUC_MIN (v);
  return res;
}

int __GIMPLE ()
reduc_and (int x)
{
  svint32_t v;
  int res;

  v = svdup_s32 (x);
  res = .REDUC_AND (v);
  return res;
}

int __GIMPLE ()
reduc_ior (int x)
{
  svint32_t v;
  int res;

  v = svdup_s32 (x);
  res = .REDUC_IOR (v);
  return res;
}

int __GIMPLE ()
reduc_xor (int x)
{
  svint32_t v;
  int res;

  v = svdup_s32 (x);
  res = .REDUC_XOR (v);
  return res;
}

/* { dg-final { scan-tree-dump-not {\.REDUC_} "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return x_} 4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 0;} 1 "forwprop1" } } */
