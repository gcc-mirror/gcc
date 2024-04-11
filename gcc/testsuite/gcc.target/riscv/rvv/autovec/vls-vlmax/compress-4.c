/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx32i __attribute__ ((vector_size (32)));
typedef int16_t vnx16i __attribute__ ((vector_size (32)));
typedef int32_t vnx8i __attribute__ ((vector_size (32)));
typedef int64_t vnx4i __attribute__ ((vector_size (32)));
typedef uint8_t vnx32ui __attribute__ ((vector_size (32)));
typedef uint16_t vnx16ui __attribute__ ((vector_size (32)));
typedef uint32_t vnx8ui __attribute__ ((vector_size (32)));
typedef uint64_t vnx4ui __attribute__ ((vector_size (32)));
typedef _Float16 vnx16f __attribute__ ((vector_size (32)));
typedef float vnx8f __attribute__ ((vector_size (32)));
typedef double vnx4f __attribute__ ((vector_size (32)));

#define MASK_4 1, 2, 5, 6
#define MASK_8 1, 2, 5, 7, 11, 12, 13, 14
#define MASK_16 0, 3, 5, 7, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
#define MASK_32                                                                \
  3, 4, 6, 7, 9, 10, 14, 15, 17, 20, 21, 22, 24, 27, 29, 31, 34, 35, 36, 37,   \
    38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49

vnx32i __attribute__ ((noinline, noclone)) test_1 (vnx32i x, vnx32i y)
{
  return __builtin_shufflevector (x, y, MASK_32);
}

vnx16i __attribute__ ((noinline, noclone)) test_2 (vnx16i x, vnx16i y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8i __attribute__ ((noinline, noclone)) test_3 (vnx8i x, vnx8i y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4i __attribute__ ((noinline, noclone)) test_4 (vnx4i x, vnx4i y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx32ui __attribute__ ((noinline, noclone)) test_5 (vnx32ui x, vnx32ui y)
{
  return __builtin_shufflevector (x, y, MASK_32);
}

vnx16ui __attribute__ ((noinline, noclone)) test_6 (vnx16ui x, vnx16ui y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8ui __attribute__ ((noinline, noclone)) test_7 (vnx8ui x, vnx8ui y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4ui __attribute__ ((noinline, noclone)) test_8 (vnx4ui x, vnx4ui y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx16f __attribute__ ((noinline, noclone)) test_9 (vnx16f x, vnx16f y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8f __attribute__ ((noinline, noclone)) test_10 (vnx8f x, vnx8f y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4f __attribute__ ((noinline, noclone)) test_11 (vnx4f x, vnx4f y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

/* { dg-final { scan-assembler-times {\tvcompress.vm} 11 } } */
/* { dg-final { scan-assembler-times {\tvslideup} 11 } } */
