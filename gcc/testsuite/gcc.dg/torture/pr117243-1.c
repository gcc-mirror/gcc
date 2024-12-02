/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

/* PR tree-optimization/117243 */
/* foo should be an infinite but sometimes it gets optimized incorrectly into
   an __builtin_unreachable(); which is not valid.  */
void
foo (unsigned int a, unsigned char b)
{
  lbl:
  for (b = 0; b <= 7; b++)
    {
      unsigned char c[1][1];
      int i, j;
      for (i = 0; i < 1; i++)
        for (j = 0; j < 1; j++)
          c[i][j] = 1;
      if (b)
	goto lbl;
    }
}

int
main ()
{
  foo (1, 2);
}

/* { dg-final { scan-tree-dump-not "__builtin_unreachable " "optimized"} } */
