/* PR tree-optimization/59594 */

#include "tree-vect.h"

#define N 1024
int b[N + 2];

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < N + 1; i++)
    {
      b[i] = i;
      asm ("");
    }
  for (i = N; i >= 0; i--)
    {
      b[i + 1] = b[i];
      b[i] = 1;
    }
  if (b[0] != 1)
    __builtin_abort ();
  for (i = 0; i < N; i++)
    if (b[i + 1] != i)
      __builtin_abort ();
  return 0;
}

