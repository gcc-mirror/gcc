/* PR tree-optimization/51581 */

#include "tree-vect.h"

#define main main1
#include "../../gcc.c-torture/execute/pr51581-1.c"
#undef main

int
main ()
{
  int i;
  check_vect ();
  asm ("");
  return main1 ();
}

/* { dg-final { cleanup-tree-dump "vect" } } */
