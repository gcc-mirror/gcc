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

int arr1[10];
int a;
int bar (struct st *s)
{
  int arr2[10];
  int b;
  if (!s)
    return 0;
  foo (&s->a);
  foo (&a);
  foo (&b);
  foo (&arr1[1]);
  foo (&arr2[1]);
}

/* { dg-final { scan-ipa-dump "Setting nonnull for 0" "cp" } } */
/* { dg-final { scan-tree-dump-times "if" 1 "vrp1" } } */
