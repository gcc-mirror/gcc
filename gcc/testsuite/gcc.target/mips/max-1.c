/* { dg-options "-march=allegrex" } */

NOMIPS16 int
foo_max (int a, int b)
{
  return (a > b) ?  a : b;
}

/* { dg-final { scan-assembler "\tmax\t" } } */
