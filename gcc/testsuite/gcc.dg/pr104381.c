/* { dg-do compile } */
/* { dg-options "-O2 -g -gtoggle -fdump-tree-optimized" } */

int foo (int x)
{
  int tem = x + 1;
  int tem2 = tem - 1;
  return tem2;
}

int
__attribute__((optimize("no-tree-pre")))
bar (int x)
{
  int tem = x + 1;
  int tem2 = tem - 1;
  return tem2;
}

// { dg-final { scan-tree-dump-not "DEBUG " "optimized" } }
