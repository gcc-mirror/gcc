/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */
int f(int a, int b)
{
  /* MAX_EXPR <a, a> should cause it to be equivalent to a.  */
  int c = a>=b?a:b;
  int d = c>=a?c:a;
  return d;
}
/* { dg-final { scan-tree-dump-times "Equivalence:" 1 "reassoc1"} } */
