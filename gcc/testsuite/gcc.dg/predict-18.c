/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int x;

short v = 0;
short expected = 0;
short max = ~0;
short m = 0;
short n = 0;
#define STRONG 0

void foo (int a, int b)
{
  if (__builtin_expect_with_probability (a < b, 1, 0.6f) > __builtin_expect (b, 0))
    global++;

  if (__builtin_expect_with_probability (a < b, 1, 0.777f) > 0)
    global++;

  if (__builtin_expect_with_probability (a < b, 1, 0.99) == __atomic_compare_exchange_n (&v, &expected, max, STRONG , __ATOMIC_RELAXED, __ATOMIC_RELAXED))
    global++;

  if (__builtin_expect_with_probability (a < 10, 1, 0.9f) > __builtin_expect_with_probability (b, 0, 0.8f))
    global++;

  if (a * __builtin_expect_with_probability (m, 0, 0.6f) > 0)
    global++;

  if (__builtin_expect_with_probability (n, 0, 0.65f) * a > 0)
    global++;
}

/* { dg-final { scan-tree-dump "combined value predictions heuristics of edge .*->.*: 54.00%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "__builtin_expect_with_probability heuristics of edge .*->.*: 77.70%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "combined value predictions heuristics of edge .*->.*: 98.96%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "combined value predictions heuristics of edge .*->.*: 71.99%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "__builtin_expect_with_probability heuristics of edge .*->.*: 40.00%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "__builtin_expect_with_probability heuristics of edge .*->.*: 35.01%" "profile_estimate"} } */
