/* PR tree-optimization/71016 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "sxtw" } } */

long int
foo (long int i)
{
  return i == 0 ? 17 : __builtin_clzl (i);
}
