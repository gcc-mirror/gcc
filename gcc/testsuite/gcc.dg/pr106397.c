/* { dg-do compile } */
/* { dg-options "-O3 -fprefetch-loop-arrays -w --param l2-cache-size=0 --param prefetch-latency=3 -fprefetch-loop-arrays" } */
/* { dg-additional-options "-march=i686 -msse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

int
bar (void)
{
  /* No return statement. */
}

__attribute__ ((simd)) int
foo (void)
{
  if (bar ())
    return 0;

  __builtin_unreachable ();
}
