/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int*
foo (int* mem, int sz, int val)
{
  int i;
#pragma GCC novector
  for (i = 0; i != sz; i++)
    if (mem[i] == val) 
      return &mem[i];
  return 0;
}

/* { dg-final { scan-tree-dump "inv_expr \[0-9\]: \\t\\(unsigned long\\) sz_\[0-9\]\\(D\\) \\* 4 \\+ \\(unsigned long\\) mem_\[0-9\]\\(D\\)" "ivopts" } } */
