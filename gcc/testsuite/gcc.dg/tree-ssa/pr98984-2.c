/* PR tree-optimization/98984 */
/* { dg-do compile { target bitint } } */
/* { dg-require-effective-target float32 } */
/* { dg-options "-O2 -std=c23 -fno-trapping-math -fdump-tree-optimized" } */
/* { dg-add-options float32 } */

signed _BitInt(25)
f (long long n)
{
  return (_Float32) n;
}

