/* { dg-do compile } */
/* { dg-options "-O2 -fno-delete-null-pointer-checks -fdump-tree-optimized -fno-isolate-erroneous-paths-dereference" }
 * */

int f(int *p)
{
  int a = *p;
  int *null = 0;
  *null = 5;
  return *p == a;
}

/* { dg-final { scan-tree-dump-times "\\\*p" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 1" "optimized" } } */


