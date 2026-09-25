/* PR127588 */
/* { dg-require-effective-target int32plus } */

#include "tree-vect.h"

int s;

__attribute__((noipa))
void foo (int init)
{
  s = init;
  do {
    s += 1074790400;
  } while (s <= 1073741823);
}

int main ()
{
  check_vect ();

  foo (-1264552634);
  if (s != 1959818566)
    __builtin_abort ();
  return 0;
}
