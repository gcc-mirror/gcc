/* PR tree-optimization/127166 */
/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

unsigned short
f (int x)
{
  return ((unsigned int) x > 65535u ? (-x) >> 31 : x);
}

/* Keep the conditional signed negation so that it is checked at runtime.  */
/* { dg-final { scan-tree-dump-not "MAX_EXPR|MIN_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump {\.UBSAN_CHECK_SUB} "optimized" } } */
