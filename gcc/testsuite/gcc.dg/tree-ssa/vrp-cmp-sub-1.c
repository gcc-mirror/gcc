/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/127485 */

unsigned fu(unsigned x, unsigned y)
{
  unsigned diff = x - y;
  if (diff > 1)
   ;
  else
    __builtin_unreachable ();
  return x == y;
}

signed fs(signed x, signed y)
{
  signed diff = x - y;
  if (diff > 1)
   ;
  else
    __builtin_unreachable ();
  return x == y;
}

/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-not " != " "optimized" } } */
