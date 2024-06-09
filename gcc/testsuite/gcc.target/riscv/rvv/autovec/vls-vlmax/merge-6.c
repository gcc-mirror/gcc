/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx4qi __attribute__((vector_size (4)));
typedef int16_t vnx2hi __attribute__((vector_size (4)));
typedef uint8_t vnx4uqi __attribute__((vector_size (4)));
typedef uint16_t vnx2uhi __attribute__((vector_size (4)));

typedef _Float16 vnx2hf __attribute__((vector_size (4)));

#define MASK_4		0, 5, 2, 7
#define MASK_2		0, 3

void __attribute__ ((noipa))
merge0 (vnx4qi x, vnx4qi y, vnx4qi *out)
{
  vnx4qi v = __builtin_shufflevector ((vnx4qi) x, (vnx4qi) y, MASK_4);
  *(vnx4qi*)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx4uqi x, vnx4uqi y, vnx4uqi *out)
{
  vnx4uqi v = __builtin_shufflevector ((vnx4uqi) x, (vnx4uqi) y, MASK_4);
  *(vnx4uqi*)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx2hi x, vnx2hi y, vnx2hi *out)
{
  vnx2hi v = __builtin_shufflevector ((vnx2hi) x, (vnx2hi) y, MASK_2);
  *(vnx2hi*)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx2uhi x, vnx2uhi y, vnx2uhi *out)
{
  vnx2uhi v = __builtin_shufflevector ((vnx2uhi) x, (vnx2uhi) y, MASK_2);
  *(vnx2uhi*)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx2hf x, vnx2hf y, vnx2hf *out)
{
  vnx2hf v = __builtin_shufflevector ((vnx2hf) x, (vnx2hf) y, MASK_2);
  *(vnx2hf*)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 5 } } */
