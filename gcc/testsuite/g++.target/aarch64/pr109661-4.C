/* { dg-options "-O2 -Wpsabi" } */


#include <stdarg.h>

#define ALIGN __attribute__((aligned(32)))

typedef __uint128_t u128_4 __attribute__((aligned(4)));
typedef __uint128_t u128_8 __attribute__((aligned(8)));
typedef __uint128_t u128_16 __attribute__((aligned(16)));
typedef __uint128_t u128_32 __attribute__((aligned(32)));
typedef __uint128_t u128;

typedef __UINT64_TYPE__ u64_4 __attribute__((aligned(4)));
typedef __UINT64_TYPE__ u64_8 __attribute__((aligned(8)));
typedef __UINT64_TYPE__ u64_16 __attribute__((aligned(16)));
typedef __UINT64_TYPE__ u64_32 __attribute__((aligned(32)));
typedef __UINT64_TYPE__ u64;

enum class ALIGN e128_4 : u128_4 { A };
enum class ALIGN e128_8 : u128_8 { A };
enum class ALIGN e128_16 : u128_16 { A };
enum class ALIGN e128_32 : u128_32 { A };
enum class ALIGN e128 : u128 { A };

enum class ALIGN e64_4 : u64_4 { A };
enum class ALIGN e64_8 : u64_8 { A };
enum class ALIGN e64_16 : u64_16 { A };
enum class ALIGN e64_32 : u64_32 { A };
enum class ALIGN e64 : u64 { A };

extern "C" {

e128_4 reg_e128_4 (int x, e128_4 y) { return y; }

e128_8 reg_e128_8 (int x, e128_8 y) { return y; }

e128_16 reg_e128_16 (int x, e128_16 y) { return y; }

e128_32 reg_e128_32 (int x, e128_32 y) { return y; }

e128 reg_e128 (int x, e128 y) { return y; }

e64_4 reg_e64_4 (int x, e64_4 y) { return y; }

e64_8 reg_e64_8 (int x, e64_8 y) { return y; }

e64_16 reg_e64_16 (int x, e64_16 y) { return y; }

e64_32 reg_e64_32 (int x, e64_32 y) { return y; }

e64 reg_e64 (int x, e64 y) { return y; }

e128_4 stack_e128_4 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_4 y) { return y; }

e128_8 stack_e128_8 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_8 y) { return y; }

e128_16 stack_e128_16 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_16 y) { return y; }

e128_32 stack_e128_32 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_32 y) { return y; }

e128 stack_e128 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128 y) { return y; }

e64_4 stack_e64_4 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_4 y) { return y; }

e64_8 stack_e64_8 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_8 y) { return y; }

e64_16 stack_e64_16 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_16 y) { return y; }

e64_32 stack_e64_32 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_32 y) { return y; }

e64 stack_e64 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64 y) { return y; }

void callee (int n, ...);

void
caller ()
{
  callee (1, e128_4::A);
  callee (1, e128_8::A);
  callee (1, e128_16::A);
  callee (1, e128_32::A);
  callee (1, e128::A);

  callee (1, e64_4::A);
  callee (1, e64_8::A);
  callee (1, e64_16::A);
  callee (1, e64_32::A);
  callee (1, e64::A);

#define LOTS 1, 2, 3, 4, 5, 6, 7, 8, 9

  callee (LOTS, e128_4::A);
  callee (LOTS, e128_8::A);
  callee (LOTS, e128_16::A);
  callee (LOTS, e128_32::A);
  callee (LOTS, e128::A);

  callee (LOTS, e64_4::A);
  callee (LOTS, e64_8::A);
  callee (LOTS, e64_16::A);
  callee (LOTS, e64_32::A);
  callee (LOTS, e64::A);
}

void
va (volatile void *ptr, ...)
{
  va_list ap;
  va_start (ap, ptr);
  *(volatile e128_4 *) ptr = va_arg (ap, e128_4);
  *(volatile e128_8 *) ptr = va_arg (ap, e128_8);
  *(volatile e128_16 *) ptr = va_arg (ap, e128_16);
  *(volatile e128_32 *) ptr = va_arg (ap, e128_32);
  *(volatile e128 *) ptr = va_arg (ap, e128);
  *(volatile e64_4 *) ptr = va_arg (ap, e64_4);
  *(volatile e64_8 *) ptr = va_arg (ap, e64_8);
  *(volatile e64_16 *) ptr = va_arg (ap, e64_16);
  *(volatile e64_32 *) ptr = va_arg (ap, e64_32);
  *(volatile e64 *) ptr = va_arg (ap, e64);
}

}
