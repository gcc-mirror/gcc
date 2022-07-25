/* { dg-do compile } */
/* { dg-options "-O3 -fprefetch-loop-arrays --param l2-cache-size=0 --param prefetch-latency=3 -fprefetch-loop-arrays" } */

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
