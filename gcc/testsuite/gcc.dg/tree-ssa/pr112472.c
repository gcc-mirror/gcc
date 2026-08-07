/* PR tree-optimization/112472 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

/* (trunc)copysign ((extend)a, CST) with a negative CST should be
   simplified to .COPYSIGN (a, -1.0e+0).  */
float f(float a)
{
  return (float)__builtin_copysign(a, -3.0);
}

/* With a non-negative CST, copysign is canonicalized to abs, so this
   becomes (float)abs((double)a) and is then simplified to abs(a),
   dropping the wider type.  */
float f2(float a)
{
  return (float)__builtin_copysign(a, 5.0);
}

/* { dg-final { scan-tree-dump-not "= __builtin_copysign" "optimized" } } */
/* { dg-final { scan-tree-dump-not " double " "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times ".COPYSIGN" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "-1.0e\\+0" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times " ABS_EXPR " 1 "optimized" { target ifn_copysign } } } */
