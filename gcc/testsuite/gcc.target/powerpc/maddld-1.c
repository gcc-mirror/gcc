/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* This file tests the maddld instruction can be used in SI mode
   on power9 machine.  */

int
s_madd (int a, int b, int c)
{
  return (a * b) + c;
}

unsigned int
u_madd (unsigned int a, unsigned int b, unsigned int c)
{
  return (a * b) + c;
}

/* { dg-final { scan-assembler-times {\mmaddld\s} 2 } } */
/* { dg-final { scan-assembler-not   {\mmul} } } */
/* { dg-final { scan-assembler-not   {\madd} } } */
