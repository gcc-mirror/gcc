/* { dg-do run } */

struct S { int *p; int *q; };

int **__attribute__((noinline,noclone,const)) foo (struct S *s)
{
  return &s->q;
}

int main()
{
  struct S s;
  int i = 1, j = 2;
  int **x;
  s.p = &i;
  s.q = &j;
  x = foo (&s);
  **x = 7;
  if (j != 7)
    __builtin_abort ();
  return 0;
}
