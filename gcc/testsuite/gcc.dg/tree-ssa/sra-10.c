/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct S
{
  int a[1];
  int z[256];
};

void foo (struct S *s, int i)
{
  struct S disappear;

  disappear.a[i] = 12;
  *s = disappear;
}

/* { dg-final { scan-tree-dump-times "disappear" 0 "optimized"} } */
