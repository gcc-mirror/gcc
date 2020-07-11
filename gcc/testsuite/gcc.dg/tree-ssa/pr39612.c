/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details -Wuninitialized" } */

void foo(int *);
void f2(int dst[3], int R)
{
  int i, inter[2];

  for (i = 1; i < R; i++) {
    if (i & 8)
      {
	inter[0] = 1;
	inter[1] = 1;
      }
  }

  foo(inter);
}

/* { dg-final { scan-tree-dump-times "Executing store motion" 2 "lim2" } } */
/* { dg-final { scan-tree-dump-not " = inter\\\[\[0-1\]\\\];" "lim2" } } */
