/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fdump-tree-optimized" } */

/* Should propagate 'a' into the assume body and fold foo.  */

inline bool
bar (int x, int y)
{
  return x < y;
}

bool
foo (int x)
{
  int a = 42;
  [[assume (bar (x, a))]];
  bool ret = x < 42;

  if (!__builtin_constant_p (ret))
    __builtin_abort ();

  return ret;
}

/* { dg-final { scan-ipa-dump "replacing param #1 a with const 42" "cp" } } */
/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
