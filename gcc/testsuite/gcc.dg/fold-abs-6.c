/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

float foo (float x, float y)
{
  return __builtin_fabsf (__builtin_copysignf (x, y));
}

/* { dg-final { scan-tree-dump "return ABS_EXPR <x>;" "original" } } */
