/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1 -fdump-tree-optimized" } */

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

/* Everything except for the "i = 0" assignment should get removed.  Value
   numbering already figures out the if in the loop is never true.  */
/* { dg-final { scan-tree-dump-times "foo" 0 "fre1" } } */
/* { dg-final { scan-tree-dump-times "if" 0 "optimized" } } */
