#include "tree-vect.h"

void __attribute__((noipa)) foo (long *) {}
void __attribute__((noipa))
d()
{
  long e[6][8][5];
  for (int b = 0; b < 6; b++)
    for (int c = 0; c < 8; c++)
      {
        e[b][c][0] = 1;
        e[b][c][1] = 1;
        e[b][c][4] = 1;
      }
  foo (&e[0][0][0]);
}
int main()
{
  check_vect ();
  d();
}
