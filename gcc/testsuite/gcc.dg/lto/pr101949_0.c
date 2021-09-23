/* { dg-lto-do run } */
/* { dg-lto-options { "-O2 -fipa-pta -flto -flto-partition=1to1" } } */

extern int bar (int (*)(int *), int *);

static int x;

static int __attribute__ ((noinline)) foo (int *p)
{
  *p = 1;
  x = 0;
  return *p;
}

int main ()
{
  if (bar (foo, &x) != 0)
    __builtin_abort ();
  return 0;
}
