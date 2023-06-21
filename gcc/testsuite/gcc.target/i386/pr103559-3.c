/* PR tree-optimization/103559 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " = .SQRT \\\(" "optimized" } } */
/* { dg-final { scan-tree-dump " = sqrtf \\\(" "optimized" } } */

float sqrtf (float);

float
foo (float x)
{
  if (!__builtin_isless (x, 0))
    __builtin_unreachable ();
  return sqrtf (x);
}
