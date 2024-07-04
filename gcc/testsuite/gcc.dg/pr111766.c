/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int
foo3n(int c, int bb)
{
  if ((bb & ~3)!=0) __builtin_unreachable(); // bb = [0,3]
  if ((bb & 1)==0) __builtin_unreachable(); // bb&1 == 0 // [0],[3]
  if(bb == 2) __builtin_trap();
  return bb;
}

/* { dg-final { scan-tree-dump-not "trap" "evrp" } } */
