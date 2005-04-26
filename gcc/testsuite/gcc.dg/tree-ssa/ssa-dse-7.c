/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

int glob1, glob2;

int foo1 (void)
{
  glob1 = 0;
  glob2 = 1;
  glob1 = 2;
  glob2 = 3;
  return glob1 + glob2;
}



/* There should only be one assignment to glob1 and glob2, the final
   return statement should just return a constant.  */
/* { dg-final { scan-tree-dump-times "glob1 = " 1 "dse1"} } */
/* { dg-final { scan-tree-dump-times "glob2 = " 1 "dse1"} } */
/* { dg-final { scan-tree-dump-times "return 5" 1 "dse1"} } */

/* { dg-final { cleanup-tree-dump "dse1" } } */
