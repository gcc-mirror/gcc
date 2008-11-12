#include <limits.h>

extern void abort (void);

static __attribute__((noinline)) void foo (int a)
{
  int b = (a - 1) + INT_MIN;

  if (b != INT_MIN)
    abort ();
}

int main (void)
{
  foo (1);
  return 0;
}
