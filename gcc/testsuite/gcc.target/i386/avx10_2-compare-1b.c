/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "comi" 6 } } */
/* { dg-final { scan-assembler-times "comx" 12 } } */

#include <immintrin.h>

int is_equal_sd (double a, double b)
{
  return a == b;
}

int is_not_equal_sd (double a, double b)
{
  return a != b;
}

int is_equal_ss (float a, float b)
{
  return a == b;
}

int is_not_equal_ss (float a, float b)
{
  return a != b;
}

int is_equal_sh (_Float16 a, _Float16 b)
{
  return a == b;
}

int is_not_equal_sh (_Float16 a, _Float16 b)
{
  return a != b;
}

int is_unordered_or_equal_sd (double a, double b)
{
  return __builtin_isunordered (a, b) || a == b;
}

int is_unordered_or_nonequal_sd (double a, double b)
{
  return __builtin_isunordered (a, b) || a != b;
}

int is_unordered_or_equal_ss (float a, float b)
{
  return __builtin_isunordered (a, b) || a == b;
}

int is_unordered_or_nonequal_ss (float a, float b)
{
  return __builtin_isunordered (a, b) || a != b;
}

int is_unordered_or_equal_sh (_Float16 a, _Float16 b)
{
  return __builtin_isunordered (a, b) || a == b;
}

int is_unordered_or_nonequal_sh (_Float16 a, _Float16 b)
{
  return __builtin_isunordered (a, b) || a != b;
}

int is_ordered_and_equal_sd (double a, double b)
{
  return !__builtin_isunordered (a, b) && a == b;
}

int is_ordered_and_nonequal_sd (double a, double b)
{
  return !__builtin_isunordered (a, b) && a != b;
}

int is_ordered_and_equal_ss (float a, float b)
{
  return !__builtin_isunordered (a, b) && a == b;
}

int is_ordered_and_nonequal_ss (float a, float b)
{
  return !__builtin_isunordered (a, b) && a != b;
}

int is_ordered_and_equal_sh (_Float16 a, _Float16 b)
{
  return !__builtin_isunordered (a, b) && a == b;
}

int is_ordered_and_nonequal_sh (_Float16 a, _Float16 b)
{
  return !__builtin_isunordered (a, b) && a != b;
}
