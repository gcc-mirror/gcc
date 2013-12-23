#include "tree-vect.h"

#define N 1024
typedef int V __attribute__((vector_size (4)));
unsigned int a[N * 2] __attribute__((aligned));
unsigned int b[N * 2] __attribute__((aligned));
V c[N];

__attribute__((noinline, noclone)) unsigned int
foo (unsigned int *a, unsigned int *b)
{
  int i;
  unsigned int r = 0;
  for (i = 0; i < N; i++)
    {
      unsigned int x = a[i], y = b[i];
      if (x < 32)
	{
	  x = x + 127;
	  y = y * 2;
	}
      else
	{
	  x = x - 16;
	  y = y + 1;
	}
      a[i] = x;
      b[i] = y;
      r += x;
    }
  return r;
}

__attribute__((noinline, noclone)) unsigned int
bar (unsigned int *a, unsigned int *b)
{
  int i;
  unsigned int r = 0;
  for (i = 0; i < N; i++)
    {
      unsigned int x = a[i], y = b[i];
      if (x < 32)
	{
	  x = x + 127;
	  y = y * 2;
	}
      else
	{
	  x = x - 16;
	  y = y + 1;
	}
      a[i] = x;
      b[i] = y;
      c[i] = c[i] + 1;
      r += x;
    }
  return r;
}

void
baz (unsigned int *a, unsigned int *b,
     unsigned int (*fn) (unsigned int *, unsigned int *))
{
  int i;
  for (i = -64; i < 0; i++)
    {
      a[i] = 19;
      b[i] = 17;
    }
  for (; i < N; i++)
    {
      a[i] = i - 512;
      b[i] = i;
    }
  for (; i < N + 64; i++)
    {
      a[i] = 27;
      b[i] = 19;
    }
  if (fn (a, b) != -512U - (N - 32) * 16U + 32 * 127U)
    __builtin_abort ();
  for (i = -64; i < 0; i++)
    if (a[i] != 19 || b[i] != 17)
      __builtin_abort ();
  for (; i < N; i++)
    if (a[i] != (i - 512U < 32U ? i - 512U + 127 : i - 512U - 16)
	|| b[i] != (i - 512U < 32U ? i * 2U : i + 1U))
      __builtin_abort ();
  for (; i < N + 64; i++)
    if (a[i] != 27 || b[i] != 19)
      __builtin_abort ();
}

int
main ()
{
  int i;
  check_vect ();
  baz (a + 512, b + 512, foo);
  baz (a + 512, b + 512, bar);
  baz (a + 512 + 1, b + 512 + 1, foo);
  baz (a + 512 + 1, b + 512 + 1, bar);
  baz (a + 512 + 31, b + 512 + 31, foo);
  baz (a + 512 + 31, b + 512 + 31, bar);
  baz (a + 512 + 1, b + 512, foo);
  baz (a + 512 + 1, b + 512, bar);
  baz (a + 512 + 31, b + 512, foo);
  baz (a + 512 + 31, b + 512, bar);
  baz (a + 512, b + 512 + 1, foo);
  baz (a + 512, b + 512 + 1, bar);
  baz (a + 512, b + 512 + 31, foo);
  baz (a + 512, b + 512 + 31, bar);
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
