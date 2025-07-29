/* { dg-do run } */
/* { dg-options "-O2" } */

#ifndef __x86_64__
#define REGPARM __attribute__((regparm(1)))
#else
#define REGPARM
#endif

extern void abort (void);

static int REGPARM
bar(void *arg)
{
  return arg != bar;
}

static int __attribute__((noinline,noclone)) REGPARM
foo(int (REGPARM **bar)(void*))
{
  return (*bar)(*bar);
}

int main()
{
  int (REGPARM *p)(void*) = bar;
  if (foo(&p))
    abort();
  return 0;
}
