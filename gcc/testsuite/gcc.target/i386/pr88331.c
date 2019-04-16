/* PR rtl-optimization/88331 */
/* { dg-do compile } */
/* { dg-options "-O3 -march=core-avx2" } */

int b, d, e, g, i, j, l, m;
int *c, *h, *n, *o;
long f, k;

void
foo (void)
{
  long p = i;
  int *a = o;
  while (p)
    {
      n = (int *) (__UINTPTR_TYPE__) a[0];
      for (; f; f += 4)
	for (; m <= d;)
	  {
	    for (; g <= e; ++g)
	      l = (int) (__UINTPTR_TYPE__) (n + l);
	    c[m] = (int) (__UINTPTR_TYPE__) n;
	  }
    }
  int q = 0;
  k = 0;
  for (; k < j; k++)
    q += o[k] * h[k];
  b = q;
}
