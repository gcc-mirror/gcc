/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx4i __attribute__ ((vector_size (4)));
typedef uint8_t vnx4ui __attribute__ ((vector_size (4)));

#define MASK_4  0, 1, 0, 1 

vnx4i __attribute__ ((noinline, noclone)) test_1 (vnx4i x, vnx4i y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

vnx4ui __attribute__ ((noinline, noclone)) test_2 (vnx4ui x, vnx4ui y)
{
  return __builtin_shufflevector (x, y, MASK_4);
}

/* { dg-final { scan-assembler-times {\tvrgather\.vi} 2 } } */
