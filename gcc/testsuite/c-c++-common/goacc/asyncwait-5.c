/* Multiple OpenACC wait clauses.  */

/* { dg-additional-options "-fdump-tree-original" } */

void f()
{
#pragma acc parallel async (1) wait (14)
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(14\\) async\\(1\\)$" 1 "original" } } */

#pragma acc parallel async (2) wait (11, 12) wait (13)
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(13\\) wait\\(12\\) wait\\(11\\) async\\(2\\)\$" 1 "original" } } */
}
