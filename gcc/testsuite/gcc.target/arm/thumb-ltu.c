/* { dg-do compile } */
/* { dg-options "-mcpu=arm1136jf-s -mthumb -O2" } */

void f(unsigned a, unsigned b, unsigned c, unsigned d)
{
  if (a <= b || c > d)
    foo();
  else
    bar();
}

/* { dg-final { scan-assembler-not "uxtb" } } */
