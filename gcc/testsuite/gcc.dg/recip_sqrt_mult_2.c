/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized -fcompare-debug" } */

float
foo (float a)
{
  float tmp = 1.0f / __builtin_sqrtf (a);
  return a * tmp;
}

/* { dg-final { scan-tree-dump-not " / " "optimized" } } */
