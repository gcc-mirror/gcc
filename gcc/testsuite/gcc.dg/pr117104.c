/* { dg-do compile } */
/* { dg-options "-O2 -fno-vect-cost-model" } */
/* { dg-additional-options "-mavx" { target { x86_64-*-* i?86-*-* } } } */

void g();
void f(long *a)
{
  long b0 = a[0] > 0 ? a[0] : 0;
  long b1 = a[1] > 0 ? a[1] : 0;
  if ((b0|b1) == 0)
    g();
}
