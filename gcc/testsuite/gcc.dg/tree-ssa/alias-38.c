/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int x;
int y;

int main ()
{
  int *volatile p = &x;
  return (p != &y);
}

/* { dg-final { scan-tree-dump " != &y" "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 1;" "optimized" } } */
