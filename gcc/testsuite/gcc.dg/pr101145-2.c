/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned foo(unsigned val, unsigned start)
{
  unsigned cnt = 0;
  unsigned i = start;
  do {
    cnt++;
    i++;
  } while (i > val);
  return cnt;
}

/* { dg-final { scan-tree-dump "cnt_\[0-9\] = start_\[0-9\]\\(D\\) >= val_\[0-9\]\\(D\\) \\? _\[0-9\] : 1;" "optimized" } } */
