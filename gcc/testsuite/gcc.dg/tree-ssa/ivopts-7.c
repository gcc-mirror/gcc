/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int*
foo (int* mem, int beg, int end, int val)
{
  int i;
  for (i = beg; i < end; i++)
    if (mem[i] == val) 
      return &mem[i];
  return 0;
}
/* { dg-final { scan-tree-dump "inv_expr \[0-9\]: \\t\\(unsigned long\\) \\(\\(unsigned int\\) end_\[0-9\]\\(D\\) - \\(unsigned int\\) beg_\[0-9\]\\(D\\)\\)" "ivopts" } } */

