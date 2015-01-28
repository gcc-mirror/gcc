/* PR tree-optimization/64277 */
/* { dg-do compile } */
/* { dg-options "-O3 -Wall -Werror -fdump-tree-cunroll-details" } */
/* { dg-final { scan-tree-dump "loop with 5 iterations completely unrolled" "cunroll" } } */
/* { dg-final { scan-tree-dump "loop with 6 iterations completely unrolled" "cunroll" } } */
/* { dg-final { cleanup-tree-dump "cunroll" } } */

int f1[10];
void test1 (short a[], short m, unsigned short l)
{
  int i = l;
  for (i = i + 5; i < m; i++)
    f1[i] = a[i]++;
}

void test2 (short a[], short m, short l)
{
  int i;
  if (m > 5)
    m = 5;
  for (i = m; i > l; i--)
    f1[i] = a[i]++;
}
