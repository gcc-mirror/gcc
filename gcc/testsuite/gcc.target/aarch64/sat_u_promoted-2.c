/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef unsigned char u8;

/* These loops expose narrow saturating operations that vectorize to one
   uqsub or uqadd.  */

void
sub_loop (u8 *__restrict d, u8 *__restrict a, u8 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] - b[i];
      d[i] = t < 0 ? 0 : t;
    }
}

void
add_loop (u8 *__restrict d, u8 *__restrict a, u8 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] + b[i];
      d[i] = t > 255 ? 255 : t;
    }
}

/* { dg-final { scan-tree-dump "\\.SAT_SUB " "optimized" } } */
/* { dg-final { scan-tree-dump "\\.SAT_ADD " "optimized" } } */
/* { dg-final { scan-assembler "uqsub\tv\[0-9\]+\.16b" } } */
/* { dg-final { scan-assembler "uqadd\tv\[0-9\]+\.16b" } } */
