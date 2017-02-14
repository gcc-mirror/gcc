/* PR tree-optimization/71016 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-final { scan-assembler-not "cltq" } } */

long int
foo (long int i)
{
  return i == 0 ? 17 : __builtin_clzl (i);
}
