/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

foo( int *a)
{
  *a = 5;
  *a = 3;
}




/* We should eliminate the first assignment to *p, but not the second.  */
/* { dg-final { scan-tree-dump-times "= 5" 0 "dse1"} } */
/* { dg-final { scan-tree-dump-times "= 3" 1 "dse1"} } */

/* { dg-final { cleanup-tree-dump "dse1" } } */


