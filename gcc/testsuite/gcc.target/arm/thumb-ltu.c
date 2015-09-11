/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-mcpu=arm1136jf-s -mthumb -O2" } */

extern int foo ();
extern int bar ();

void f(unsigned a, unsigned b, unsigned c, unsigned d)
{
  if (a <= b || c > d)
    foo();
  else
    bar();
}

/* { dg-final { scan-assembler-not "uxtb" } } */
