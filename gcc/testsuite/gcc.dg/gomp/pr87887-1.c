/* PR middle-end/87887 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-w" } */

struct S { int n; };
#pragma omp declare simd
struct S
foo (int x)
{
  return (struct S) { x };
}
/* { dg-warning "unsupported return type ‘struct S’ for ‘simd’ functions" "" { target aarch64*-*-* } .-4 } */

#pragma omp declare simd
int
bar (struct S x)
{
  return x.n;
}
/* { dg-warning "unsupported argument type ‘struct S’ for ‘simd’ functions" "" { target aarch64*-*-* } .-4 } */

#pragma omp declare simd uniform (x)
int
baz (int w, struct S x, int y)
{
  return w + x.n + y;
}
