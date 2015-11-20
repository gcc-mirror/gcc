/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target avx_runtime } } */

int a, b;

void
fn1 ()
{
  int c, d;
  for (; b; b++)
    a = a ^ !c ^ !d;
}
