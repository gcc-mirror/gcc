/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void foo(int k)
{
  int i = 1;
  void *label;

  label = k ? &&x : &&y;

  if (k == 1)
    goto *label;

  i = 0;
  goto z;
z:
x:
  if (i)
    dont_remove ();
y: ;
}

/* { dg-final { scan-tree-dump-times "dont_remove \\(\\)" 1 "optimized"} } */
