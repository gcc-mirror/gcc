/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdbool.h>

bool my_uadd_overflow (unsigned short a, unsigned short b, unsigned short *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_usub_overflow (unsigned short a, unsigned short b, unsigned short *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_uneg_overflow (unsigned short a, unsigned short *res)
{
  return __builtin_sub_overflow (0, a, res);
}

bool my_add_overflow (short a, short b, short *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_sub_overflow (short a, short b, short *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_neg_overflow (short a, short *res)
{
  return __builtin_sub_overflow (0, a, res);
}

/* { dg-final { scan-assembler-times "add.w" 2 } } */
/* { dg-final { scan-assembler-times "sub.w" 4 } } */
/* { dg-final { scan-assembler-not "cmp.w" } } */
