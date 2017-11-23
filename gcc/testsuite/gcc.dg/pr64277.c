/* PR tree-optimization/64277 */
/* { dg-do compile } */
/* { dg-options "-O3 -Wall -Werror -fdump-tree-cunroll-details" } */
/* { dg-final { scan-tree-dump "loop with 4 iterations completely unrolled" "cunroll" } } */
/* { dg-final { scan-tree-dump "loop with 5 iterations completely unrolled" "cunroll" } } */

#if __SIZEOF_INT__ < 4
  __extension__ typedef __INT32_TYPE__ int32_t;
#else
  typedef int int32_t;
#endif

int32_t f1[10];
void test1 (short a[], short m, unsigned short l)
{
  int32_t i = l;
  for (i = i + 5; i < m; i++)
    f1[i] = a[i]++;
}

void test2 (short a[], short m, short l)
{
  int32_t i;
  if (m > 5)
    m = 5;
  for (i = m; i > l; i--)
    f1[i] = a[i]++;
}
