/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int f (int a, int b)
{
  int x = -a;
  int y = b - x;
  return y;
}

/* We tested for reassociation to -(a + b) on the following which
   isn't a transform that makes things cheaper.  With reassoc
   no longer applying to types with undefined overflow we lost
   this transform.

int g (int a, int b)
{
  int x = -a;
  int y = x - b;
  return y;
}

*/

/* There should be an addition now.  */
/* { dg-final { scan-tree-dump-times "\\+" 1 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
