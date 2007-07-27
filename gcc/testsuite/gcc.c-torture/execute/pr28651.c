#include <limits.h>

extern void abort (void);
int __attribute__((noinline))
foo (unsigned int u)
{
  return (int)(u + 4) < (int)u;
}

int
main (int argc, char *argv[])
{
  unsigned int u = INT_MAX;

  if (foo (u) == 0)
    abort();
  return 0;
}

