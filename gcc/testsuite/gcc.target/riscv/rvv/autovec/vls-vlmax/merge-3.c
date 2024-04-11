/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx64qi __attribute__((vector_size (64)));
typedef int16_t vnx32hi __attribute__((vector_size (64)));
typedef int32_t vnx16si __attribute__((vector_size (64)));
typedef int64_t vnx8di __attribute__((vector_size (64)));
typedef uint8_t vnx64uqi __attribute__((vector_size (64)));
typedef uint16_t vnx32uhi __attribute__((vector_size (64)));
typedef uint32_t vnx16usi __attribute__((vector_size (64)));
typedef uint64_t vnx8udi __attribute__((vector_size (64)));

typedef _Float16 vnx32hf __attribute__((vector_size (64)));
typedef float vnx16sf __attribute__((vector_size (64)));
typedef double vnx8df __attribute__((vector_size (64)));

#define MASK_64		0, 65, 2, 67, 4, 69, 6, 71, 8, 73,			\
			  10, 75, 12, 77, 14, 79, 16, 81, 18, 83, 		\
			  20, 85, 22, 87, 24, 89, 26, 91, 28, 93, 30, 95,    \
        32, 97, 34, 99, 36, 101, 38, 103, 40, 105,  \
        42, 107, 44, 109, 46, 111, 48, 113, 50, 115, \
        52, 117, 54, 119, 56, 121, 58, 123, 60, 125, \
        62, 127
#define MASK_32		0, 33, 2, 35, 4, 37, 6, 39, 8, 41,			\
			  10, 43, 12, 45, 14, 47, 16, 49, 18, 51, 		\
			  20, 53, 22, 55, 24, 57, 26, 59, 28, 61, 30, 63
#define MASK_16		0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31
#define MASK_8		0, 9, 2, 11, 4, 13, 6, 15

void __attribute__ ((noipa))
merge0 (vnx64qi x, vnx64qi y, vnx64qi *out)
{
  vnx64qi v = __builtin_shufflevector ((vnx64qi) x, (vnx64qi) y, MASK_64);
  *(vnx64qi*)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx64uqi x, vnx64uqi y, vnx64uqi *out)
{
  vnx64uqi v = __builtin_shufflevector ((vnx64uqi) x, (vnx64uqi) y, MASK_64);
  *(vnx64uqi*)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx32hi x, vnx32hi y, vnx32hi *out)
{
  vnx32hi v = __builtin_shufflevector ((vnx32hi) x, (vnx32hi) y, MASK_32);
  *(vnx32hi*)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx32uhi x, vnx32uhi y, vnx32uhi *out)
{
  vnx32uhi v = __builtin_shufflevector ((vnx32uhi) x, (vnx32uhi) y, MASK_32);
  *(vnx32uhi*)out = v;
}

void __attribute__ ((noipa))
merge4 (vnx16si x, vnx16si y, vnx16si *out)
{
  vnx16si v = __builtin_shufflevector ((vnx16si) x, (vnx16si) y, MASK_16);
  *(vnx16si*)out = v;
}

void __attribute__ ((noipa))
merge5 (vnx16usi x, vnx16usi y, vnx16usi *out)
{
  vnx16usi v = __builtin_shufflevector ((vnx16usi) x, (vnx16usi) y, MASK_16);
  *(vnx16usi*)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx8di x, vnx8di y, vnx8di *out)
{
  vnx8di v = __builtin_shufflevector ((vnx8di) x, (vnx8di) y, MASK_8);
  *(vnx8di*)out = v;
}

void __attribute__ ((noipa))
merge7 (vnx8udi x, vnx8udi y, vnx8udi *out)
{
  vnx8udi v = __builtin_shufflevector ((vnx8udi) x, (vnx8udi) y, MASK_8);
  *(vnx8udi*)out = v;
}

void __attribute__ ((noipa))
merge8 (vnx32hf x, vnx32hf y, vnx32hf *out)
{
  vnx32hf v = __builtin_shufflevector ((vnx32hf) x, (vnx32hf) y, MASK_32);
  *(vnx32hf*)out = v;
}

void __attribute__ ((noipa))
merge9 (vnx16sf x, vnx16sf y, vnx16sf *out)
{
  vnx16sf v = __builtin_shufflevector ((vnx16sf) x, (vnx16sf) y, MASK_16);
  *(vnx16sf*)out = v;
}

void __attribute__ ((noipa))
merge10 (vnx8df x, vnx8df y, vnx8df *out)
{
  vnx8df v = __builtin_shufflevector ((vnx8df) x, (vnx8df) y, MASK_8);
  *(vnx8df*)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 11 } } */
