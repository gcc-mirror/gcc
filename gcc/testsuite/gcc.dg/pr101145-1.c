/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned foo(unsigned val, unsigned start)
{
  unsigned cnt = 0;
  for (unsigned i = start; i > val; ++i)
    cnt++;
  return cnt;
}

/* { dg-final { scan-tree-dump "cnt_\[0-9\] = -start_\[0-9\]\\(D\\);" "optimized" } } */
