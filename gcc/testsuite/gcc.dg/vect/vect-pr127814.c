/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

int bf;

__attribute__((noipa))
int get_bf () { return 5; }

__attribute__((noipa))
void test (int x)
{
  int cb = -996736171;
  if (x == 0)
    cb = 0;
  bf = get_bf ();
  while (1)
    {
      bf -= 2;
      if (-520093677 * bf >= cb)
	break;
    }
}

int main ()
{
  check_vect ();

  test (1);
  if (bf != 1)
    __builtin_abort ();
  return 0;
}
