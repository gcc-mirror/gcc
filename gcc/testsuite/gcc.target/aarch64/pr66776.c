/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */

unsigned long long
foo (unsigned int a, unsigned int b, unsigned int c)
{
  return a ? b : c;
}

/* { dg-final { scan-assembler-not "uxtw" } } */
