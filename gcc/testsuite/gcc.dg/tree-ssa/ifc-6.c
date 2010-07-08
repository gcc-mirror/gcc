/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize" { target *-*-* } } */

static int x;
foo (int n, int *A)
{
  int i;
  for (i = 0; i < n; i++)
    {
      if (A[i])
	x = 2;
      if (A[i + 1])
	x = 1;
    }
}
