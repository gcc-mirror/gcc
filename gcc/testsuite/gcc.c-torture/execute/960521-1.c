/* { dg-add-options stack_size } */

#include <stdlib.h>

int *a, *b;
int n;

#ifdef STACK_SIZE
#define BLOCK_SIZE (STACK_SIZE / (sizeof (*a) + sizeof (*b)))
#else
#define BLOCK_SIZE 32768
#endif
void
foo (void)
{
  int i;
  for (i = 0; i < n; i++)
    a[i] = -1;
  for (i = 0; i < BLOCK_SIZE - 1; i++)
    b[i] = -1;
}

int
main (void)
{
  n = BLOCK_SIZE;
  a = malloc (n * sizeof(*a));
  b = malloc (n * sizeof(*b));
  *b++ = 0;
  foo ();
  if (b[-1])
    abort ();
  exit (0);
}
