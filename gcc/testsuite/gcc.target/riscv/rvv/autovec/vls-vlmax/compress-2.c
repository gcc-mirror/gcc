/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx8i __attribute__ ((vector_size (8)));
typedef int16_t vnx4i __attribute__ ((vector_size (8)));
typedef uint8_t vnx8ui __attribute__ ((vector_size (8)));
typedef uint16_t vnx4ui __attribute__ ((vector_size (8)));
typedef _Float16 vnx4f __attribute__ ((vector_size (8)));

#define MASK_4 1, 3, 6, 7
#define MASK_8 2, 3, 5, 6, 11, 12, 13, 14

vnx8i __attribute__ ((noinline, noclone))
test_1 (vnx8i x, vnx8i y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4i __attribute__ ((noinline, noclone))
test_2 (vnx4i x, vnx4i y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx8ui __attribute__ ((noinline, noclone))
test_3 (vnx8ui x, vnx8ui y)
{
  return __builtin_shufflevector (x, y, MASK_8);
}

vnx4ui __attribute__ ((noinline, noclone))
test_4 (vnx4ui x, vnx4ui y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx4f __attribute__ ((noinline, noclone))
test_5 (vnx4f x, vnx4f y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

/* { dg-final { scan-assembler-times {\tvcompress\.vm} 5 } } */
/* { dg-final { scan-assembler-times {\tvslideup} 2 } } */
