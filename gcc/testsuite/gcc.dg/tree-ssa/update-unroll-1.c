/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O1 -fprefetch-loop-arrays -march=amdfam10 -fdump-tree-aprefetch-blocks" } */

int a[10000];

int foo(unsigned n)
{
  unsigned i, s = 0;

  for (i = 0; i < n; i++)
    s += a[i];

  return s;
}

/* We used to make the probability that the body of the loop (unrolled
   to enable prefetching) is entered 0, which is not correct.  */

/* { dg-final { scan-tree-dump-not "Invalid sum" "aprefetch"} } */
/* { dg-final { scan-tree-dump-not "SUCC: 7 .100.0%" "aprefetch"} } */
