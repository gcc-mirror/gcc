/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void link_error (void);
extern void abort (void);

int *t;
int __attribute__((noinline)) g(int *a)
{
  t = a;
  *a = 2;
}

void __attribute__((noinline)) f(int *a)
{
  int b;
  b = 1;
  g(&b);
  b = 2;
  *a = 1;
  if (b != 2)
    link_error();
}

int main(void)
{
  int t;
  f(&t);
  if (t != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
