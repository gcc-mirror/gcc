#include <stdlib.h>
#include "tree-vect.h"

struct s { int x; } __attribute__((packed));

void __attribute__((noipa))
f (char *xc, char *yc, int z)
{
  for (int i = 0; i < 100; ++i)
    {
      struct s *x = (struct s *) xc;
      struct s *y = (struct s *) yc;
      x->x += y->x;
      xc += z;
      yc += z;
    }
}

int main ()
{
  check_vect ();
  char *x = malloc (100 * sizeof (struct s) + 1);
  char *y = malloc (100 * sizeof (struct s) + 1);
  f (x + 1, y + 1, sizeof (struct s));
  return 0;
}
