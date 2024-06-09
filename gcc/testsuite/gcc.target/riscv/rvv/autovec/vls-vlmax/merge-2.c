/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx32qi __attribute__((vector_size (32)));
typedef int16_t vnx16hi __attribute__((vector_size (32)));
typedef int32_t vnx8si __attribute__((vector_size (32)));
typedef int64_t vnx4di __attribute__((vector_size (32)));
typedef uint8_t vnx32uqi __attribute__((vector_size (32)));
typedef uint16_t vnx16uhi __attribute__((vector_size (32)));
typedef uint32_t vnx8usi __attribute__((vector_size (32)));
typedef uint64_t vnx4udi __attribute__((vector_size (32)));

typedef _Float16 vnx16hf __attribute__((vector_size (32)));
typedef float vnx8sf __attribute__((vector_size (32)));
typedef double vnx4df __attribute__((vector_size (32)));

#define MASK_32		0, 33, 2, 35, 4, 37, 6, 39, 8, 41,			\
			  10, 43, 12, 45, 14, 47, 16, 49, 18, 51, 		\
			  20, 53, 22, 55, 24, 57, 26, 59, 28, 61, 30, 63 
#define MASK_16		0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31 
#define MASK_8		0, 9, 2, 11, 4, 13, 6, 15 
#define MASK_4		0, 5, 2, 7 

void __attribute__ ((noipa))
merge0 (vnx32qi x, vnx32qi y, vnx32qi *out)
{
  vnx32qi v = __builtin_shufflevector ((vnx32qi) x, (vnx32qi) y, MASK_32);
  *(vnx32qi*)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx32uqi x, vnx32uqi y, vnx32uqi *out)
{
  vnx32uqi v = __builtin_shufflevector ((vnx32uqi) x, (vnx32uqi) y, MASK_32);
  *(vnx32uqi*)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx16hi x, vnx16hi y, vnx16hi *out)
{
  vnx16hi v = __builtin_shufflevector ((vnx16hi) x, (vnx16hi) y, MASK_16);
  *(vnx16hi*)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx16uhi x, vnx16uhi y, vnx16uhi *out)
{
  vnx16uhi v = __builtin_shufflevector ((vnx16uhi) x, (vnx16uhi) y, MASK_16);
  *(vnx16uhi*)out = v;
}

void __attribute__ ((noipa))
merge4 (vnx8si x, vnx8si y, vnx8si *out)
{
  vnx8si v = __builtin_shufflevector ((vnx8si) x, (vnx8si) y, MASK_8);
  *(vnx8si*)out = v;
}

void __attribute__ ((noipa))
merge5 (vnx8usi x, vnx8usi y, vnx8usi *out)
{
  vnx8usi v = __builtin_shufflevector ((vnx8usi) x, (vnx8usi) y, MASK_8);
  *(vnx8usi*)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx4di x, vnx4di y, vnx4di *out)
{
  vnx4di v = __builtin_shufflevector ((vnx4di) x, (vnx4di) y, MASK_4);
  *(vnx4di*)out = v;
}

void __attribute__ ((noipa))
merge7 (vnx4udi x, vnx4udi y, vnx4udi *out)
{
  vnx4udi v = __builtin_shufflevector ((vnx4udi) x, (vnx4udi) y, MASK_4);
  *(vnx4udi*)out = v;
}

void __attribute__ ((noipa))
merge8 (vnx16hf x, vnx16hf y, vnx16hf *out)
{
  vnx16hf v = __builtin_shufflevector ((vnx16hf) x, (vnx16hf) y, MASK_16);
  *(vnx16hf*)out = v;
}

void __attribute__ ((noipa))
merge9 (vnx8sf x, vnx8sf y, vnx8sf *out)
{
  vnx8sf v = __builtin_shufflevector ((vnx8sf) x, (vnx8sf) y, MASK_8);
  *(vnx8sf*)out = v;
}

void __attribute__ ((noipa))
merge10 (vnx4df x, vnx4df y, vnx4df *out)
{
  vnx4df v = __builtin_shufflevector ((vnx4df) x, (vnx4df) y, MASK_4);
  *(vnx4df*)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 11 } } */
