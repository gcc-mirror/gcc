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

struct st s2;
int a;
int bar (struct st *s)
{
  struct st s3;
  int b;
  if (!s)
    return 0;
  foo (&s->a);
  foo (&s2.a);
  foo (&s3.a);
  foo (&a);
  foo (&b);
}

/* { dg-final { scan-ipa-dump "Setting value range.* \\\[1, \\+INF\\\]" "cp" } } */
/* { dg-final { scan-tree-dump-times "if" 1 "vrp1" } } */
