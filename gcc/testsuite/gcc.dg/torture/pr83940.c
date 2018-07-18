/* { dg-do compile } */
/* { dg-options "-mavx512f" { target { i?86-*-* x86_64-*-* } } } */

void
foo (double *a[], int b)
{
  for (; b; b++)
    a[b][b] = 1.0;
}
