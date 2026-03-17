/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-forward-propagate -fnon-call-exceptions -fno-dse -fprofile-generate -fopenmp -finstrument-functions" } */
/* { dg-additional-options "-mno-outline-atomics" { target aarch64*-*-* } } */

/* PR rtl-optimization/124454 */

int a, b;

void
foo()
{
  long c;
  a *= b;
  b = *(char *)__builtin_memset(&c, 1, 4);
}
