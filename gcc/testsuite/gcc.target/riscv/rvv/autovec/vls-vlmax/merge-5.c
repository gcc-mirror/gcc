/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx8qi __attribute__((vector_size (8)));
typedef int16_t vnx4hi __attribute__((vector_size (8)));
typedef int32_t vnx2si __attribute__((vector_size (8)));
typedef uint8_t vnx8uqi __attribute__((vector_size (8)));
typedef uint16_t vnx4uhi __attribute__((vector_size (8)));
typedef uint32_t vnx2usi __attribute__((vector_size (8)));

typedef _Float16 vnx4hf __attribute__((vector_size (8)));
typedef float vnx2sf __attribute__((vector_size (8)));

#define MASK_8		0, 9, 2, 11, 4, 13, 6, 15
#define MASK_4		0, 5, 2, 7
#define MASK_2		0, 3

void __attribute__ ((noipa))
merge0 (vnx8qi x, vnx8qi y, vnx8qi *out)
{
  vnx8qi v = __builtin_shufflevector ((vnx8qi) x, (vnx8qi) y, MASK_8);
  *(vnx8qi *)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx8uqi x, vnx8uqi y, vnx8uqi *out)
{
  vnx8uqi v = __builtin_shufflevector ((vnx8uqi) x, (vnx8uqi) y, MASK_8);
  *(vnx8uqi *)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx4hi x, vnx4hi y, vnx4hi *out)
{
  vnx4hi v = __builtin_shufflevector ((vnx4hi) x, (vnx4hi) y, MASK_4);
  *(vnx4hi *)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx4uhi x, vnx4uhi y, vnx4uhi *out)
{
  vnx4uhi v = __builtin_shufflevector ((vnx4uhi) x, (vnx4uhi) y, MASK_4);
  *(vnx4uhi *)out = v;
}

void __attribute__ ((noipa))
merge4 (vnx2si x, vnx2si y, vnx2si *out)
{
  vnx2si v = __builtin_shufflevector ((vnx2si) x, (vnx2si) y, MASK_2);
  *(vnx2si *)out = v;
}

void __attribute__ ((noipa))
merge5 (vnx2usi x, vnx2usi y, vnx2usi *out)
{
  vnx2usi v = __builtin_shufflevector ((vnx2usi) x, (vnx2usi) y, MASK_2);
  *(vnx2usi *)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx4hf x, vnx4hf y, vnx4hf *out)
{
  vnx4hf v = __builtin_shufflevector ((vnx4hf) x, (vnx4hf) y, MASK_4);
  *(vnx4hf *)out = v;
}

void __attribute__ ((noipa))
merge7 (vnx2sf x, vnx2sf y, vnx2sf *out)
{
  vnx2sf v = __builtin_shufflevector ((vnx2sf) x, (vnx2sf) y, MASK_2);
  *(vnx2sf *)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 8 } } */
