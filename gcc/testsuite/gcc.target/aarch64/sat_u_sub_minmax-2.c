/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned int u32;

/* The recognition must reach the vectoriser, so that the loop becomes a
   single uqsub rather than a compare, a select and a subtraction.  */

void
min_loop (u8 *__restrict d, u8 *__restrict a, u8 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      u8 x = a[i], y = b[i];
      d[i] = x - (x < y ? x : y);
    }
}

void
min_loop32 (u32 *__restrict d, u32 *__restrict a, u32 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      u32 x = a[i], y = b[i];
      d[i] = x - (y < x ? y : x);
    }
}

/* { dg-final { scan-tree-dump "\\.SAT_SUB " "optimized" } } */
/* { dg-final { scan-assembler "uqsub\tv\[0-9\]+\\.16b" } } */
/* { dg-final { scan-assembler "uqsub\tv\[0-9\]+\\.4s" } } */
