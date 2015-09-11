/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */
int foo (int b, int j)
{
  if (b)
    {
      int i;
      for (i = 0; i<1000; ++i)
        ;
      j = 0;
    }
  return j;
}
/* Check that empty loop is eliminated in this case.  We should no longer have
   the exit condition after the loop.  */
/* { dg-final { scan-tree-dump-not "999" "cddce1"} } */
/* { dg-final { scan-tree-dump-not "1000" "cddce1"} } */

