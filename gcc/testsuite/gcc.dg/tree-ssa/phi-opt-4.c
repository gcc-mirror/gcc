/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

_Bool t();
_Bool t1();

_Bool f1()
{
  return t() && t1();
}

/* There should be only one if, the outer one; the inner one
   should have been changed to straight line code with the
   value of b (except that we don't fold ! (b != 0) into b
   which means that we miss a sib calling opportunity).  */
/* { dg-final { scan-tree-dump-times "if " 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
