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


#pragma acc parallel async (3) wait
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(-1\\) async\\(3\\)$" 1 "original" } } */

#pragma acc parallel async (4) wait (100) wait
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(-1\\) wait\\(100\\) async\\(4\\)$" 1 "original" } } */

#pragma acc parallel async (5) wait wait (101)
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(101\\) wait\\(-1\\) async\\(5\\)$" 1 "original" } } */

#pragma acc parallel async (6) wait wait (102, 103) wait wait
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(-1\\) wait\\(-1\\) wait\\(103\\) wait\\(102\\) wait\\(-1\\) async\\(6\\)$" 1 "original" } } */

#pragma acc parallel async (7) wait (104) wait wait (105, 106)
  ;
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel wait\\(106\\) wait\\(105\\) wait\\(-1\\) wait\\(104\\) async\\(7\\)$" 1 "original" } } */
}
