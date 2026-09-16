/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fdump-ipa-sra-details -fdump-tree-optimized" } */

/* Should propagate baz -> foo -> assume, split the parameter, derive the range
   for s->x and fold foo accordingly.  */

void something ();

struct S
{
  int x, y;
};

inline bool
bar (int x, int y)
{
  return x < y;
}

static bool __attribute__ ((noinline))
foo (struct S *s)
{
  [[assume (bar (s->x, s->y))]];
  bool ret = s->x < 42;
  bool ahoj = s->x < s->y;

  if (!__builtin_constant_p (ret))
    __builtin_abort ();

  if (!__builtin_constant_p (ahoj))
    __builtin_abort ();

  return ret;
}

void
baz (int x)
{
  struct S s;
  s.x = x;
  s.y = 42;
  bool shouldnt_do_something = foo (&s);

  if (!shouldnt_do_something)
    something ();
}

/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[4]=42" "cp" } } */
/* { dg-final { scan-ipa-dump ".*assume.*constprop.* Will split parameter 0" "sra" } } */
/* { dg-final { scan-ipa-dump "assume.*constprop\\.isra" "sra" } } */
/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
