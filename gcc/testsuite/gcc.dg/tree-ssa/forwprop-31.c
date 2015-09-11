/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ccp -fdump-tree-forwprop1" } */

int foo (int x)
{
  int y = 0;
  int z = x + 1;
  int w = z + y; /* becomes z */
  return w - z; /* becomes 0 */
}

/* The original y = 0 stmt is also retained.  */
/* { dg-final { scan-tree-dump-times "= 0;" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "-" 0 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\+" 1 "forwprop1" } } */
