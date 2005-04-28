/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

_Bool f1(_Bool a, _Bool b)
{
  if (a)
   {
     if (b)
      return 1;
     else
      return 0;
   }
  return 0;
}


/* There should be only one if, the outer one; the inner one
   should have been changed to straight line code with the
   value of b (except that we don't fold ! (b != 0) into b
   which can be fixed in a different patch).  */
/* { dg-final { scan-tree-dump-times "if" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
