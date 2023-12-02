/* { dg-do run { target { int128 && { ! ia32 } } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-O2" } */

#include <stdlib.h>

#define APX_TARGET __attribute__((noinline, target("apxf")))
#define NO_APX __attribute__((noinline, target("no-apxf")))
typedef __uint128_t u128;
typedef __int128 i128;

#define TI_SHIFT_FUNC(TYPE, op, name) \
APX_TARGET \
TYPE apx_##name##TYPE (TYPE a, char b) \
{ \
  return a op b; \
} \
TYPE noapx_##name##TYPE (TYPE a, char b) \
{ \
  return a op b; \
} \

#define TI_SHIFT_FUNC_CONST(TYPE, i, op, name) \
APX_TARGET \
TYPE apx_##name##TYPE##_const (TYPE a) \
{ \
  return a op i; \
} \
NO_APX \
TYPE noapx_##name##TYPE##_const (TYPE a) \
{ \
  return a op i; \
}

#define TI_SHIFT_TEST(TYPE, name, val) \
{\
  if (apx_##name##TYPE (val, b) != noapx_##name##TYPE (val, b)) \
    abort (); \
}

#define TI_SHIFT_CONST_TEST(TYPE, name, val) \
{\
  if (apx_##name##1##TYPE##_const (val) \
      != noapx_##name##1##TYPE##_const (val)) \
    abort (); \
  if (apx_##name##2##TYPE##_const (val) \
      != noapx_##name##2##TYPE##_const (val)) \
    abort (); \
  if (apx_##name##3##TYPE##_const (val) \
      != noapx_##name##3##TYPE##_const (val)) \
    abort (); \
  if (apx_##name##4##TYPE##_const (val) \
      != noapx_##name##4##TYPE##_const (val)) \
    abort (); \
}

TI_SHIFT_FUNC(i128, <<, ashl)
TI_SHIFT_FUNC(i128, >>, ashr)
TI_SHIFT_FUNC(u128, >>, lshr)

TI_SHIFT_FUNC_CONST(i128, 1, <<, ashl1)
TI_SHIFT_FUNC_CONST(i128, 65, <<, ashl2)
TI_SHIFT_FUNC_CONST(i128, 64, <<, ashl3)
TI_SHIFT_FUNC_CONST(i128, 87, <<, ashl4)
TI_SHIFT_FUNC_CONST(i128, 127, >>, ashr1)
TI_SHIFT_FUNC_CONST(i128, 87, >>, ashr2)
TI_SHIFT_FUNC_CONST(i128, 27, >>, ashr3)
TI_SHIFT_FUNC_CONST(i128, 64, >>, ashr4)
TI_SHIFT_FUNC_CONST(u128, 127, >>, lshr1)
TI_SHIFT_FUNC_CONST(u128, 87, >>, lshr2)
TI_SHIFT_FUNC_CONST(u128, 27, >>, lshr3)
TI_SHIFT_FUNC_CONST(u128, 64, >>, lshr4)

int main (void)
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  u128 ival = 0x123456788765432FLL;
  u128 uval = 0xF234567887654321ULL;
  char b = 28;

  TI_SHIFT_TEST(i128, ashl, ival)
  TI_SHIFT_TEST(i128, ashr, ival)
  TI_SHIFT_TEST(u128, lshr, uval)
  TI_SHIFT_CONST_TEST(i128, ashl, ival)
  TI_SHIFT_CONST_TEST(i128, ashr, ival)
  TI_SHIFT_CONST_TEST(u128, lshr, uval)

  return 0;
}
