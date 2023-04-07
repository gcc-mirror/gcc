/* PR tree-optimization/103559 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " = sqrtf \\\(" "optimized" } } */

float sqrtf (float);

float
foo (float x)
{
  x = x * x;
  return sqrtf (x);
}
