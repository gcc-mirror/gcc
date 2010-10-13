/* { dg-do run } */

#include <stdint.h>

extern void abort (void);

uintptr_t __attribute__((pure,noinline,noclone))
foo (int *a)
{
  return (uintptr_t) a;
}

void __attribute__((noinline,noclone))
bar (uintptr_t a)
{
  int *p = (int *)a;
  *p = 1;
}

int main()
{
  int t = 0;
  bar (foo (&t));
  if (t != 1)
    abort ();
  return 0;
}
