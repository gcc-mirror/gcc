/* PR rtl-optimization/55010 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

long long int a;
unsigned long long int b;

void
foo (void)
{
  a = (a < 0) / ((a -= b) ? b >= ((b = a) || 0) : 0);
}
