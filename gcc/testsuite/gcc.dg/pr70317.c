/* PR tree-optimization/70317 */
/* { dg-do compile } */
/* { dg-skip-if "No NaN support" { spu*-*-* vax*-*-* pdp11*-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef double V __attribute__((vector_size (256)));
typedef __typeof ((V) {} < (V) {}) T;
T a, b;

__attribute__((noinline, noclone, optimize ("finite-math-only"))) void
foo (V *x)
{
  V z = *x;
  a = z <= z;
}

/* { dg-final { scan-tree-dump "a\[^\n\r]*= . -1, -1," "optimized" } } */

__attribute__((noinline, noclone, optimize ("no-finite-math-only"))) void
bar (V *x)
{
  V z = *x;
  b = z <= z;
}

/* { dg-final { scan-tree-dump-not "b\[^\n\r]*= . -1, -1," "optimized" } } */
