/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */
/* { dg-require-effective-target c99_runtime } */

#pragma GCC target ("arch=armv8.2-a+fp16")

_Float16 f (_Float16 x) 
{
  _Float16 t1 = __builtin_sqrt (x);
  _Float16 t2 = x / t1;
  return t2;
}

/* { dg-final { scan-tree-dump "gimple_simplified to t2_\[0-9\]+ = .SQRT .x_\[0-9\]*.D.." "forwprop1" } } */
