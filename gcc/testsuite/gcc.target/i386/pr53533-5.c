/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times {(?n)\* 2147483647} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {(?n)\* 1073741823} 1 "optimized" } } */

#define INT_MAX 2147483647
int
foo (int a)
{
  /* When a == -2, there's no overflow for (a + 1) * INT_MAX - 1.
     but overflow for a * INT_MAX + (INT_MAX - 1).
     Don't simpify it.  */
  return (a + 1) * INT_MAX - 1;
}

int
foo1 (int a)
{
  /* Be conservative here, don't simplify this as long as
     a * 2147483646 may overflow.  */
  return 1073741823 * (a * 2 + 1);
}
