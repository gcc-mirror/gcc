/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

int a, b;

void
fn1 ()
{
  int c, d;
  for (; b; b++)
    a = a ^ !c ^ !d;
}
