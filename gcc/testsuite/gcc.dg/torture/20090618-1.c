/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

extern void abort (void);

struct X { int *p; int *q; };

int foo(void)
{
  int i = 0, j = 1;
  struct X x, y;
  int **p;
  y.p = &i;
  x.q = &j;
  p = __builtin_mempcpy (&x, &y, sizeof (int *));
  return **p;
}

int main()
{
  if (foo() != 1)
    abort ();
  return 0;
}
