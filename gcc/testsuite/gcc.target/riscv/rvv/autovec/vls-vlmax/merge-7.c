/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx2qi __attribute__((vector_size (2)));
typedef uint8_t vnx2uqi __attribute__((vector_size (2)));

#define MASK_2		0, 3

void __attribute__ ((noipa))
merge0 (vnx2qi x, vnx2qi y, vnx2qi *out)
{
  vnx2qi v = __builtin_shufflevector ((vnx2qi) x, (vnx2qi) y, MASK_2);
  *(vnx2qi *)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx2uqi x, vnx2uqi y, vnx2uqi *out)
{
  vnx2uqi v = __builtin_shufflevector ((vnx2uqi) x, (vnx2uqi) y, MASK_2);
  *(vnx2uqi *)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 2 } } */
