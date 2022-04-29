/* PR rtl-optimization/105314 */
/* { dg-do compile } *
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tbeq\t" } } */

long
foo (long a, long b, long c)
{
  if (c)
    a = 0;
  return a;
}
