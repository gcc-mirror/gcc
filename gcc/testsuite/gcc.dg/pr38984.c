/* { dg-do compile } */
/* { dg-options "-O2 -fno-delete-null-pointer-checks -fdump-tree-optimized" }
 * */

int f(int *p)
{
  int a = *p;
  int *null = 0;
  *null = 5;
  return *p == a;
}

/* Currently fails because of PR38985.  */

/* { dg-final { scan-tree-dump-times " = \\\*p" 2 "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "return 1" "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */


