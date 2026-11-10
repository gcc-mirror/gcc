/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -mapxf -fdump-rtl-ira" } */

/* B0-B2 are referenced only in the inner loop, which stays a region, so
   they live through the outer loop without a reference in it.  */

extern void f (long, long, long, long);
extern void g (long, long, long, long);

void
foo (long *a, long *b, long n, long m)
{
  long a0 = a[0], a1 = a[1], a2 = a[2];
  long b0 = b[0], b1 = b[1], b2 = b[2];
  for (long i = 0; i < n; i++)
    {
      f (i, a0, a1, a2);
      for (long j = 0; j < m; j++)
	g (j, b0, b1, b2);
    }
}

/* { dg-final { scan-rtl-dump {Loop 2 flatten-cost:[^\n]*keep} "ira" } } */
/* { dg-final { scan-rtl-dump {Loop 1 flatten-cost:[^\n]*keep} "ira" } } */
