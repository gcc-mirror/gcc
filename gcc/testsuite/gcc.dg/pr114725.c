/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

//* This should fold to return 0
bool
src(int offset)
{
  if (offset > 128)
    return 0;
  else
    return (offset & -9) == 258;
}

/* { dg-final { scan-tree-dump "return 0"  "optimized" } } */
/* { dg-final { scan-tree-dump-not "PHI"  "optimized" } } */
