/* { dg-do run } */
/* { dg-options "-fipa-pta" } */

int **x;

static int __attribute__((noinline,noclone))
foo (int **p)
{
  int a = 1;
  **p = 0;
  *x = &a;
  return **p;
}

extern void abort (void);
int main()
{
  int b;
  int *p = &b;
  x = &p;
  if (foo (&p) != 1)
    abort ();
  return 0;
}
