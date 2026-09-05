/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef unsigned int u32;

/* Recognition lets the loop vectorize to one uqadd.  */

void
f (u32 *__restrict d, u32 *__restrict a, u32 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = a[i] > 0xffffffffu - b[i] ? 0xffffffffu : a[i] + b[i];
}

/* { dg-final { scan-tree-dump "\\.SAT_ADD " "optimized" } } */
/* { dg-final { scan-assembler "uqadd\tv\[0-9\]+\.4s" } } */
