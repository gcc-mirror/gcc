/* { dg-do compile } */
/* { dg-options "-Os -fno-tree-fre -fdump-tree-dom2" } */

void foo(int *);
void f2(int dst[3], int R)
{
  int i, inter[2];
  _Bool inter0p = 0;
  _Bool inter1p = 0;
  for (i = 1; i < R; i++)
    {
      inter0p = 1;
      inter1p = 1;
    }
  if (inter0p)
    inter[0] = 1;
  if (inter1p)
    inter[1] = 1;
  foo(inter);
}


/* There should be precisely two conditionals.  One for the loop condition
   and one for the test after the loop.  Previously we failed to eliminate
   the second conditional after the loop.  */
/* { dg-final { scan-tree-dump-times "if" 2 "dom2"} } */


