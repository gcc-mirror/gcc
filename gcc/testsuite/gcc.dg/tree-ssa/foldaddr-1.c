/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-original" } */


char *a;
int foo(char *b)
{
        return a+5+(long)b == (long)b+a;
}

/* Folding should have determined that the two addresses were
   not identical and thus collapsed the function into a trivial
   "return 0".  */
/* { dg-final { scan-tree-dump-times "return 0" 1 "original"} } */
/* { dg-final { cleanup-tree-dump "original" } } */

