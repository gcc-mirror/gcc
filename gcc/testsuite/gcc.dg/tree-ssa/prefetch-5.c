/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=amdfam10 --param min-insn-to-prefetch-ratio=5 -fdump-tree-aprefetch-details" } */

/* These are common idioms for writing variable-length arrays at the end
   of structures.  We should not deduce anything about the number of iterations
   of the loops from them.  */

struct tail0
{
  int xxx;
  int yyy[0];
};

int loop0 (int n, struct tail0 *x)
{
  int i, s = 0;

  for (i = 0; i < n; i++)
    s += x->yyy[i];

  return s;
}

struct tail1
{
  int xxx;
  int yyy[1];
};
int loop1 (int n, struct tail1 *x)
{
  int i, s = 0;

  for (i = 0; i < n; i++)
    s += x->yyy[i];

  return s;
}

/* It is unlikely that this should be a tail array.  We may deduce that most
   likely, the loop iterates about 5 times, and the array is not worth prefetching.  */

struct tail5
{
  int xxx;
  int yyy[5];
};
int loop5 (int n, struct tail5 *x)
{
  int i, s = 0;

  for (i = 0; i < n; i++)
    s += x->yyy[i];

  return s;
}

/* Until we are able to track likely upper bounds, we can't really work out that
   small trailing arrays should not be prefetched.  */
/* { dg-final { scan-tree-dump-times "Issued prefetch" 2 "aprefetch"  } } */
/* { dg-final { scan-tree-dump-times "Not prefetching" 1 "aprefetch"  } } */
