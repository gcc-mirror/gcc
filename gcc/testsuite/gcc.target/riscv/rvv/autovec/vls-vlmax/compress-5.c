/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx64i __attribute__ ((vector_size (64)));
typedef int16_t vnx32i __attribute__ ((vector_size (64)));
typedef int32_t vnx16i __attribute__ ((vector_size (64)));
typedef int64_t vnx8i __attribute__ ((vector_size (64)));
typedef uint8_t vnx64ui __attribute__ ((vector_size (64)));
typedef uint16_t vnx32ui __attribute__ ((vector_size (64)));
typedef uint32_t vnx16ui __attribute__ ((vector_size (64)));
typedef uint64_t vnx8ui __attribute__ ((vector_size (64)));
typedef _Float16 vnx32f __attribute__ ((vector_size (64)));
typedef float vnx16f __attribute__ ((vector_size (64)));
typedef double vnx8f __attribute__ ((vector_size (64)));

#define MASK_4 0, 2, 5, 6
#define MASK_8 0, 1, 5, 7, 11, 12, 13, 14
#define MASK_16 0, 2, 3, 8, 10, 11, 12, 14, 24, 25, 26, 27, 28, 29, 30, 31
#define MASK_32                                                                \
  4, 5, 6, 7, 9, 10, 12, 14, 18, 20, 22, 23, 25, 27, 28, 29, 35, 36, 37, 38,   \
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50
#define MASK_64                                                                \
  1, 2, 3, 5, 7, 9, 10, 11, 12, 14, 15, 17, 19, 21, 22, 23, 26, 28, 30, 31,    \
    37, 38, 41, 46, 47, 53, 54, 55, 60, 61, 62, 63, 76, 77, 78, 79, 80, 81,    \
    82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,    \
    100, 101, 102, 103, 104, 105, 106, 107

vnx64i __attribute__ ((noinline, noclone)) test_1 (vnx64i x, vnx64i y)
{
  return __builtin_shufflevector (x, y, MASK_64);
}

vnx32i __attribute__ ((noinline, noclone)) test_2 (vnx32i x, vnx32i y)
{
  return __builtin_shufflevector (x, y, MASK_32);
}

vnx16i __attribute__ ((noinline, noclone)) test_3 (vnx16i x, vnx16i y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8i __attribute__ ((noinline, noclone)) test_4 (vnx8i x, vnx8i y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx64ui __attribute__ ((noinline, noclone)) test_5 (vnx64ui x, vnx64ui y)
{
  return __builtin_shufflevector (x, y, MASK_64);
}

vnx32ui __attribute__ ((noinline, noclone)) test_6 (vnx32ui x, vnx32ui y)
{
  return __builtin_shufflevector (x, y, MASK_32);
}

vnx16ui __attribute__ ((noinline, noclone)) test_7 (vnx16ui x, vnx16ui y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8ui __attribute__ ((noinline, noclone)) test_8 (vnx8ui x, vnx8ui y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx32f __attribute__ ((noinline, noclone)) test_9 (vnx32f x, vnx32f y)
{
  return __builtin_shufflevector (x, y, MASK_32);
}

vnx16f __attribute__ ((noinline, noclone)) test_10 (vnx16f x, vnx16f y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8f __attribute__ ((noinline, noclone)) test_11 (vnx8f x, vnx8f y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

/* { dg-final { scan-assembler-times {\tvcompress.vm} 11 } } */
