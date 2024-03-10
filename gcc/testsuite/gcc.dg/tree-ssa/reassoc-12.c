/* { dg-do compile } */ 
/* Match-and-simplify can handle now MAX<MAX<a,b>,a>->MAX<a,b>, disable all of the passes that uses that. */
/* { dg-options "-O1 -fdump-tree-reassoc1-details -fno-tree-ccp -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre" } */
int f(int a, int b)
{
  /* MAX_EXPR <a, a> should cause it to be equivalent to a.  */
  int c = a>=b?a:b;
  int d = c>=a?c:a;
  return d;
}
/* { dg-final { scan-tree-dump-times "Equivalence:" 1 "reassoc1"} } */
