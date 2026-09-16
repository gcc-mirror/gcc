/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fdump-tree-optimized" } */

/* Should propagate in chain baz -> foo -> assume and fold body of foo
   accordingly.  Works because bar can be inlined (otherwise we can't derive the
   value range for x).  */

void do_something ();

inline bool
bar (int x, int y)
{
  return x < y;
}

static bool __attribute__ ((noinline))
foo (int x, int a)
{
  [[assume (bar (x, a))]];
  bool ret = x < 42;

  if (!__builtin_constant_p (ret))
    __builtin_abort ();

  return ret;
}

void
baz (int x)
{
  bool should_do_something = false;
  for (int i = 0; i < 1000000; i++)
    should_do_something = foo (x, 42);

  if (!should_do_something)
    do_something ();
}

/* { dg-final { scan-ipa-dump-times "replacing param #1 a with const 42" 2 "cp" } } */
/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
