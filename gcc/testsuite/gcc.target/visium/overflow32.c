/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdbool.h>

bool my_uadd_overflow (unsigned int a, unsigned int b, unsigned int *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_usub_overflow (unsigned int a, unsigned int b, unsigned int *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_uneg_overflow (unsigned int a, unsigned int *res)
{
  return __builtin_sub_overflow (0, a, res);
}

bool my_add_overflow (int a, int b, int *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_sub_overflow (int a, int b, int *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_neg_overflow (int a, int *res)
{
  return __builtin_sub_overflow (0, a, res);
}

/* { dg-final { scan-assembler-times "add.l" 2 } } */
/* { dg-final { scan-assembler-times "sub.l" 4 } } */
/* { dg-final { scan-assembler-not "cmp.l" } } */
/* { dg-final { scan-assembler-not "mov.l" } } */
