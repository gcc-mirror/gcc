/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "call\[\\t ]*.s.\[\\t ]*.f" } } */
/* { dg-final { scan-assembler-not "call\[\\t ]*.s.\[\\t ]*.g" } } */

extern void f () __attribute__ ((weak));
extern void g () __attribute__ ((weak)) __attribute__ ((noinline));

void g ()
{
}

int main ()
{
  f ();
  g ();
}
