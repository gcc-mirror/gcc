#include "tree-vect.h"

struct st
{
  unsigned int num : 8;
};

void __attribute__((noipa))
mem_overlap (struct st *a, struct st *b)
{
  for (int i = 0; i < 9; i++)
    a[i].num = b[i].num + 1;
}

int
main (void)
{
  check_vect ();

  struct st a[9] = {};

  // input a = 0, 0, 0, 0, 0, 0, 0, 0, 0
  mem_overlap (&a[1], a);

  // output a = 0, 1, 2, 3, 4, 5, 6, 7, 8
  if (a[2].num == 2)
    return 0;
  else
    __builtin_abort ();
}
