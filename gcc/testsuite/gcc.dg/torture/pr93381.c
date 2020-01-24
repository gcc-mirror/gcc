/* { dg-do run } */

static struct S { int *p1; int *p2; } s;
typedef __UINTPTR_TYPE__ uintptr_t;
int foo()
{
  int i = 1, j = 2;
  struct S s;
  int **p;
  s.p1 = &i;
  s.p2 = &j;
  p = &s.p1;
  uintptr_t pi = (uintptr_t)p;
  pi = pi + sizeof (int *);
  p = (int **)pi;
  **p = 3;
  return j;
}

int main()
{
  if (foo () != 3)
    __builtin_abort ();
  return 0;
}
