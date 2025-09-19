/* { dg-options "-march=allegrex" } */

NOMIPS16 int
foo_min (int a, int b)
{
  return (a < b) ?  a : b;
}

/* { dg-final { scan-assembler "\tmin\t" } } */
