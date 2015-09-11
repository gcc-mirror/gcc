/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized -fno-partial-inlining" } */

/* When optimizing for size, t should be inlined when it expands to one call only.  */
extern int q(int);
int t(int a)
{
  if (a > 12)
    {
      q(a+5);
      q(a+5);
    }
  else
      q(a+10);
}

int
main()
{
   t(5);
   t(20);
}
/* { dg-final { scan-tree-dump-times "q \\(15\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "t \\(20\\)" 1 "optimized" } } */
