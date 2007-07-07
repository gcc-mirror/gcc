/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

 int i; int foo(void) { i = 2; int j = i * 2; int k = i + 2; return j == k; }
/* { dg-final { scan-tree-dump-times "Replaced " 5 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
