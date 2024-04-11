/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx16i __attribute__ ((vector_size (16)));
typedef int16_t vnx8i __attribute__ ((vector_size (16)));
typedef int32_t vnx4i __attribute__ ((vector_size (16)));
typedef uint8_t vnx16ui __attribute__ ((vector_size (16)));
typedef uint16_t vnx8ui __attribute__ ((vector_size (16)));
typedef uint32_t vnx4ui __attribute__ ((vector_size (16)));
typedef _Float16 vnx8f __attribute__ ((vector_size (16)));
typedef float vnx4f __attribute__ ((vector_size (16)));

#define MASK_4 0, 2, 5, 6
#define MASK_8 0, 1, 2, 5, 10, 11, 12, 13
#define MASK_16 1, 3, 4, 6, 7, 8, 14, 15, 20, 21, 22, 23, 24, 25, 26, 27

vnx16i __attribute__ ((noinline, noclone)) test_1 (vnx16i x, vnx16i y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8i __attribute__ ((noinline, noclone)) test_2 (vnx8i x, vnx8i y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4i __attribute__ ((noinline, noclone)) test_3 (vnx4i x, vnx4i y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx16ui __attribute__ ((noinline, noclone)) test_4 (vnx16ui x, vnx16ui y)
{
  return __builtin_shufflevector (x, y, MASK_16);
}

vnx8ui __attribute__ ((noinline, noclone)) test_5 (vnx8ui x, vnx8ui y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4ui __attribute__ ((noinline, noclone)) test_6 (vnx4ui x, vnx4ui y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx8f __attribute__ ((noinline, noclone)) test_7 (vnx8f x, vnx8f y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4f __attribute__ ((noinline, noclone)) test_8 (vnx4f x, vnx4f y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

/* { dg-final { scan-assembler-times {\tvcompress.vm} 8 } } */
/* { dg-final { scan-assembler-times {\tvslideup} 8 } } */
