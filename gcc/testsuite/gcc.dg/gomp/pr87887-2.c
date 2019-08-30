/* PR middle-end/87887 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target vect_simd_clones } */

struct S { int n; };
#pragma omp declare simd
struct S
foo (int x)		/* { dg-warning "unsupported return type 'struct S' for simd" } */
{
  return (struct S) { x };
}

#pragma omp declare simd
int
bar (struct S x)	/* { dg-warning "unsupported argument type 'struct S' for simd" } */
{
  return x.n;
}

#pragma omp declare simd uniform (x)
int
baz (int w, struct S x, int y)
{
  return w + x.n + y;
}
