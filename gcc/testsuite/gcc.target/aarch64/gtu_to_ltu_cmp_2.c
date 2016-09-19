/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int
foo (unsigned int a, unsigned int b)
{
  return (a + 10) > 9;
}

/* { dg-final { scan-assembler-times "cmn\\tw\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-not "add\\tw\[0-9\]+" } } */
