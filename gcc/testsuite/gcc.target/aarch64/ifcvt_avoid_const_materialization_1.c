/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that we avoid moving the immediate into a register
   if comparison has shown that the inverse or negated form is
   already in one of the registers.  */

int
foo (int a, int b)
{
  return a == 5 ? -5 : b;
}

int
bar (int a, int b)
{
  return a != 5 ? b : ~5;
}

/* { dg-final { scan-assembler-not "mov\\tw\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "csneg\\tw\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "csinv\\tw\[0-9\]+" 1 } } */
