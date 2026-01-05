/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fno-partial-inlining -fdump-ipa-cp-details" } */

struct S
{
  int a, b, m, n, o, p;
};


void __attribute__ ((noinline))
foo (struct S *p, double *x, double *y, double *z)
{
  if (p->a == 0 && p->b == 0)
    return;

  /* Something long so that we do not clone unless we know it will
     disappear: */
  for (int i = 0; i < p->m; i++)
    {
      if (i < p->m-1)
	x[i] += x[i+1] * y[0];
      for (int j = 0; j < p->n; j++)
	for (int k = 0; j < p->o; k++)
	  {
	    double s = *z;
	    for (int l = 1; l < p->p; l++)
	      s += z[l];
	    x[i] += s + 2*y[j] + 7*z[k];
	  }
    }
}

int get_int (void);

void
  entry (int c, double *x, double *y, double *z)
{
  struct S s;

  s.a = 0;
  s.b = 0;
  s.m = get_int ();
  s.n = get_int ();
  s.o = get_int ();
  s.p = get_int ();
  foo (&s, x, y, z);
}

/* { dg-final { scan-ipa-dump "Creating a specialized node of foo" "cp" } } */
