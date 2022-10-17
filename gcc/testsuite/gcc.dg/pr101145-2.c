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

/* Look for start >= val ? -start : 1  */
/* { dg-final { scan-tree-dump " = start_\[0-9\]\\(D\\) >= val_\[0-9\]\\(D\\);" "optimized" } } */
/* { dg-final { scan-tree-dump "cnt_\[0-9\] = _\[0-9\]+ \\? _\[0-9\]+ : 1;" "optimized" } } */
