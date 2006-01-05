/* A test for induction variable merging.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void foo(long);

void xxx(void)
{
  long iter, jter;

  for (iter = 0, jter = 2; iter < 100; iter++, jter++)
    {
      foo (iter);
      foo (jter);
    }
}

/* Only iter variable should remain.  */

/* { dg-final { scan-tree-dump-times "int iter" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "jter" 0 "optimized" } } */

/* And the use of jter should be replaced by iter + 2 */

/* { dg-final { scan-tree-dump-times " \\+ 2" 1 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
