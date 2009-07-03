/* PR 30730, PR 26900, number of iterations analysis should be able to
   determine number of iterations of the following loops unconditionally.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstrict-overflow -fdump-tree-optimized-blocks" } */

unsigned foo(unsigned int n)
{
  unsigned x = 0;;

  while (n > 10)
    {
      n -= 2;
      x++;
    }

  return x;
}

int foo0(int i0, int i1)
{
  int i, j = 0;
  for (i=i0; i<=i1+1; ++i)
    ++j;
  return j;
}

/* { dg-final { scan-tree-dump-times "if" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
