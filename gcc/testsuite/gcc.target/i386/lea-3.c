/* { dg-do compile } */
/* { dg-options "-O2" } */

int m;

int foo(int y)
{
  return (m+y-1)/y;
}

/* { dg-final { scan-assembler "leal" } } */
/* { dg-final { scan-assembler-not "addl" } } */
/* { dg-final { scan-assembler-not "subl" } } */
