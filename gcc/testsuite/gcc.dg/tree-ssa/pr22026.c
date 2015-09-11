/* PR tree-optimization/22026
   VRP used think that ~[0,0] + ~[0,0] = ~[0,0], which is wrong.  The
   same applies to subtraction and unsigned multiplication.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int
plus (int x, int y)
{
  if (x != 0)
    if (y != 0)
      {
        int z = x + y;
        if (z != 0)
          return 1;
      }
  return 0;
}

int
minus (int x, int y)
{
  if (x != 0)
    if (y != 0)
      {
        int z = x - y;
        if (z != 0)
          return 1;
      }
  return 0;
}

int
mult (unsigned x, unsigned y)
{
  if (x != 0)
    if (y != 0)
      {
	unsigned z = x * y;
	if (z != 0)
	  return 1;
      }
  return 0;
}

/* None of the predicates can be folded in these functions.  */
/* { dg-final { scan-tree-dump-times "Folding predicate" 0 "vrp1" } } */
