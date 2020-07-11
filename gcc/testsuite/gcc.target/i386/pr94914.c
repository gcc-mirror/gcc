/* PR tree-optimization/94914 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "\tseto\t" } } */
/* { dg-final { scan-assembler "\tsetno\t" } } */

int
foo (unsigned int x, unsigned int y)
{
  return (((unsigned long long)x * y) >> 32) != 0;
}

int
bar (unsigned int x, unsigned int y)
{
  return (((unsigned long long)x * y) >> 32) == 0;
}
