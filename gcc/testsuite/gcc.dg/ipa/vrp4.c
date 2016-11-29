/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp-details -fdump-tree-vrp1" } */

static __attribute__((noinline, noclone))
int foo (int *p)
{
  if (!p)
    return 0;
  *p = 1;
}

struct st
{
  int a;
  int b;
};

int bar (struct st *s)
{

  if (!s)
    return 0;
  foo (&s->a);
  foo (&s->b);
}

/* { dg-final { scan-ipa-dump "Setting nonnull for 0" "cp" } } */
/* { dg-final { scan-tree-dump-times "if" 1 "vrp1" } } */
