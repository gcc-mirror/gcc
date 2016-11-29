/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdbool.h>

bool my_uadd_overflow (unsigned char a, unsigned char b, unsigned char *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_usub_overflow (unsigned char a, unsigned char b, unsigned char *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_uneg_overflow (unsigned char a, unsigned char *res)
{
  return __builtin_sub_overflow (0, a, res);
}

bool my_add_overflow (signed char a, signed char b, signed char *res)
{
  return __builtin_add_overflow (a, b, res);
}

bool my_sub_overflow (signed char a, signed char b, signed char *res)
{
  return __builtin_sub_overflow (a, b, res);
}

bool my_neg_overflow (signed char a, signed char *res)
{
  return __builtin_sub_overflow (0, a, res);
}

/* { dg-final { scan-assembler-times "add.b" 2 } } */
/* { dg-final { scan-assembler-times "sub.b" 4 } } */
/* { dg-final { scan-assembler-not "cmp.b" } } */
/* { dg-final { scan-assembler-not "mov.b" } } */
