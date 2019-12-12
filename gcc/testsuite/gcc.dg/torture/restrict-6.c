/* { dg-do run } */

extern void abort (void);

void __attribute__((noinline)) g(int **a, int *b)
{
  *a = b;
}

int foo(int * restrict p, int *q)
{
  g(&q, p);
  *p = 1;
  *q = 2;
  return *p + *q;
}

int main()
{
  int x, y;
  if (foo(&x, &y) != 4)
    abort ();
  return 0;
}
