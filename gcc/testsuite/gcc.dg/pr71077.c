/* PR c++/71077  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

void
foo (int *a, int n)
{
  int b, c;
  for (b = 0; b < n; b++)
    for (c = 0; c < 32; c++)
      if ((b & 1U) << c)
	a[b + c] = 0;
}
