/* { dg-do run } */

struct S { int i; void *p; int j; };
int a;
int __attribute__((noinline))
foo (struct S * __restrict p, int q)
{
  int *x = &p->j;
  if (q)
    x = &a;
  p->j = 1;
  *x = 2;
  return p->j;
}

int main()
{
  struct S s;
  if (foo (&s, 0) != 2)
    __builtin_abort ();
  return 0;
}
