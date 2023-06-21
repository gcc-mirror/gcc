/* { dg-do compile } */
/* { dg-options "-Ofast --param tree-reassoc-width=2 -fdump-tree-widening_mul" } */
/* { dg-additional-options "-mfpmath=sse -mfma" { target { i?86-*-* x86_64-*-* } } } */

/* Test that the compiler properly optimizes multiply and add 
   to generate more FMA instructions.  */
#define N 1024
double a[N];
double b[N];
double c[N];
double d[N];
double e[N];
double f[N];
double g[N];
double h[N];
double j[N];
double k[N];
double l[N];
double m[N];
double o[N];
double p[N];


void
foo (void)
{
  for (int i = 0; i < N; i++)
  {
    a[i] += b[i] * c[i] + d[i] * e[i] + f[i] * g[i] + h[i] * j[i] + k[i] * l[i] + m[i]* o[i] + p[i];
  }
}
/* { dg-final { scan-tree-dump-times { = \.FMA \(} 6 "widening_mul" { target { i?86-*-* x86_64-*-* } } } } */
