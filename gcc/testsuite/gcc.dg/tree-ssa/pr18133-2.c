/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-blocks" } */

int c, d;

int
bar (int a)
{
  void *p;
  int b;

  if (a!=0)
    {
      b = 3;
      p = &&L0;
    }
  else
    {
      b = 5;
      p = &&L1;
    }

  goto *p;

 L0:
  c = b;
  return 1;

 L1:
  d = b;
  return 0;
}

/* The both paths to the block containing the goto *p should
   have been threaded, thus eliminating the need for the goto *p.  */

/* { dg-final { scan-tree-dump-times "goto p" 0 "optimized" } } */

/* There should not be any abnormal edges as DOM removed the
   computed goto.  */

/* { dg-final { scan-tree-dump-times "ab" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
