#include "tree-vect.h"

short b = 2;

int main()
{
  check_vect ();

  for (int a = 1; a <= 9; a++)
    b = b * b;
  if (b != 0)
    __builtin_abort ();

  return 0;
}

