/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/127485 */


unsigned fu(unsigned x, unsigned y)
{
  unsigned diff = x - y;
  if (diff >= 1)
   ;
  else
    __builtin_unreachable ();
  if (x == y) diff ++ ;
  return diff;
}

signed fs(signed x, signed y)
{
  signed diff = x - y;
  if (diff != 0)
   ;
  else
    __builtin_unreachable ();
  if (x == y) diff ++ ;
  return diff;
}

/* { dg-final { scan-tree-dump-not " if " "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-not " != " "optimized" } } */
