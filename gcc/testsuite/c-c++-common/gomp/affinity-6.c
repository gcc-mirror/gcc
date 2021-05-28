/* { dg-additional-options "-fdump-tree-gimple" } */
int bar (int);
int bar2 (int);

void foobar()
{
  int d[64], e[64], f[64];
#pragma omp parallel default(none)  /* { dg-note "enclosing 'parallel'" }  */
#pragma omp task affinity (d, e[bar(5)], f[4:10])
  ;
/* { dg-error "'f' not specified in enclosing 'parallel'" "" { target *-*-* } .-2 }  */
/* { dg-error "'e' not specified in enclosing 'parallel'" "" { target *-*-* } .-3 }  */
/* { dg-error "'d' not specified in enclosing 'parallel'" "" { target *-*-* } .-4 }  */
}

void
foo (void)
{
  int a[64];
#pragma omp parallel default(none)  /* { dg-note "enclosing 'parallel'" }  */
#pragma omp task affinity (iterator (j=bar(0):bar(1):bar(2))  : a[bar(j)])
  ;
/* { dg-error "'a' not specified in enclosing 'parallel'" "" { target *-*-* } .-2 }  */
}

void
qux (void)
{
  int a[64], b[64], c[64];
#pragma omp parallel default(none)  /* { dg-note "enclosing 'parallel'" }  */
#pragma omp task affinity (iterator (j=bar(0):bar(1):bar(2))  : a[bar(j+1)], b[bar(j+2)], c[bar(j+3)])
  ;
/* { dg-error "'a' not specified in enclosing 'parallel'" "" { target *-*-* } .-2 }  */
/* { dg-error "'c' not specified in enclosing 'parallel'" "" { target *-*-* } .-3 }  */
/* { dg-error "'b' not specified in enclosing 'parallel'" "" { target *-*-* } .-4 }  */
}
