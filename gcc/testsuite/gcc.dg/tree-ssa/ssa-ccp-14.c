/* PR tree-optimization/29738.  We used not to realize that "i" can never
   become nonzero.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int i;

void foo (void);
void bar (void)
{
  int j;
  i = 0;
  for (j = 0; j < 10000; j++)
    if (i)
      foo ();
}

/* Everything except for the "i = 0" assignment should get removed.  */
/* { dg-final { scan-tree-dump-times "if" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
