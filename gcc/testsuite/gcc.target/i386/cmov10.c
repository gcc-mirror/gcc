/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -dp" } */
/* { dg-final { scan-assembler-not "zero_extendsidi" } } */


void foo (unsigned long long *d, int a, unsigned int b, unsigned int c)
{
  *d = a ? b : c;
}

