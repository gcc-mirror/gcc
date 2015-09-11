/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

#include "vrp.h"

void test1(int i, int j)
{
  RANGE(i, 1, 5);
  RANGE(j, 7, 10);
  CHECK_RANGE(i + j, 8, 15);
}

#define UINT_MAX 2*(unsigned)__INT_MAX__ + 1
void test2(unsigned int i)
{
  RANGE(i, UINT_MAX - 0x4, UINT_MAX - 0x1);
  CHECK_ANTI_RANGE(i + 0x2, 0x1, UINT_MAX - 0x3);
}
void test3(unsigned int i)
{
  RANGE(i, UINT_MAX - 0x4, UINT_MAX - 0x1);
  CHECK_RANGE(i + 0x5, 0x0, 0x3);
}
void test4(unsigned int i)
{
  RANGE(i, 2, 4);
  CHECK_ANTI_RANGE(i - 4, 1, UINT_MAX - 2);
}
void test5(unsigned int i)
{
  RANGE(i, 2, 4);
  CHECK_RANGE(i - 8, UINT_MAX - 5, UINT_MAX - 3);
}

int main() {}

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp1" } } */
