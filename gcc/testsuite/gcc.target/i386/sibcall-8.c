/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

static int __attribute__((regparm(1)))
bar(void *arg)
{
  return arg != bar;
}

static int __attribute__((noinline,noclone,regparm(1)))
foo(int (__attribute__((regparm(1))) **bar)(void*))
{
  return (*bar)(*bar);
}

int main()
{
  int (__attribute__((regparm(1))) *p)(void*) = bar;
  if (foo(&p))
    abort();
  return 0;
}
