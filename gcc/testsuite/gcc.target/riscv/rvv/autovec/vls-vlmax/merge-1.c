/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx16qi __attribute__((vector_size (16)));
typedef int16_t vnx8hi __attribute__((vector_size (16)));
typedef int32_t vnx4si __attribute__((vector_size (16)));
typedef int64_t vnx2di __attribute__((vector_size (16)));
typedef uint8_t vnx16uqi __attribute__((vector_size (16)));
typedef uint16_t vnx8uhi __attribute__((vector_size (16)));
typedef uint32_t vnx4usi __attribute__((vector_size (16)));
typedef uint64_t vnx2udi __attribute__((vector_size (16)));

typedef _Float16 vnx8hf __attribute__((vector_size (16)));
typedef float vnx4sf __attribute__((vector_size (16)));
typedef double vnx2df __attribute__((vector_size (16)));

#define MASK_16		0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31
#define MASK_8		0, 9, 2, 11, 4, 13, 6, 15
#define MASK_4		0, 5, 2, 7
#define MASK_2		0, 3

void __attribute__ ((noipa))
merge0 (vnx16qi x, vnx16qi y, vnx16qi *out)
{
  vnx16qi v = __builtin_shufflevector ((vnx16qi) x, (vnx16qi) y, MASK_16);
  *(vnx16qi*)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx16uqi x, vnx16uqi y, vnx16uqi *out)
{
  vnx16uqi v = __builtin_shufflevector ((vnx16uqi) x, (vnx16uqi) y, MASK_16);
  *(vnx16uqi*)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx8hi x, vnx8hi y, vnx8hi *out)
{
  vnx8hi v = __builtin_shufflevector ((vnx8hi) x, (vnx8hi) y, MASK_8);
  *(vnx8hi*)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx8uhi x, vnx8uhi y, vnx8uhi *out)
{
  vnx8uhi v = __builtin_shufflevector ((vnx8uhi) x, (vnx8uhi) y, MASK_8);
  *(vnx8uhi*)out = v;
}

void __attribute__ ((noipa))
merge4 (vnx4si x, vnx4si y, vnx4si *out)
{
  vnx4si v = __builtin_shufflevector ((vnx4si) x, (vnx4si) y, MASK_4);
  *(vnx4si*)out = v;
}

void __attribute__ ((noipa))
merge5 (vnx4usi x, vnx4usi y, vnx4usi *out)
{
  vnx4usi v = __builtin_shufflevector ((vnx4usi) x, (vnx4usi) y, MASK_4);
  *(vnx4usi*)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx2di x, vnx2di y, vnx2di *out)
{
  vnx2di v = __builtin_shufflevector ((vnx2di) x, (vnx2di) y, MASK_2);
  *(vnx2di*)out = v;
}

void __attribute__ ((noipa))
merge7 (vnx2udi x, vnx2udi y, vnx2udi *out)
{
  vnx2udi v = __builtin_shufflevector ((vnx2udi) x, (vnx2udi) y, MASK_2);
  *(vnx2udi*)out = v;
}

void __attribute__ ((noipa))
merge8 (vnx8hf x, vnx8hf y, vnx8hf *out)
{
  vnx8hf v = __builtin_shufflevector ((vnx8hf) x, (vnx8hf) y, MASK_8);
  *(vnx8hf*)out = v;
}

void __attribute__ ((noipa))
merge9 (vnx4sf x, vnx4sf y, vnx4sf *out)
{
  vnx4sf v = __builtin_shufflevector ((vnx4sf) x, (vnx4sf) y, MASK_4);
  *(vnx4sf*)out = v;
}

void __attribute__ ((noipa))
merge10 (vnx2df x, vnx2df y, vnx2df *out)
{
  vnx2df v = __builtin_shufflevector ((vnx2df) x, (vnx2df) y, MASK_2);
  *(vnx2df*)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 11 } } */
