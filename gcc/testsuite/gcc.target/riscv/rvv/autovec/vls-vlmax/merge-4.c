/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx128qi __attribute__((vector_size (128)));
typedef int16_t vnx64hi __attribute__((vector_size (128)));
typedef int32_t vnx32si __attribute__((vector_size (128)));
typedef int64_t vnx16di __attribute__((vector_size (128)));
typedef uint8_t vnx128uqi __attribute__((vector_size (128)));
typedef uint16_t vnx64uhi __attribute__((vector_size (128)));
typedef uint32_t vnx32usi __attribute__((vector_size (128)));
typedef uint64_t vnx16udi __attribute__((vector_size (128)));

typedef _Float16 vnx64hf __attribute__((vector_size (128)));
typedef float vnx32sf __attribute__((vector_size (128)));
typedef double vnx16df __attribute__((vector_size (128)));

#define MASK_128		0, 129, 2, 131, 4, 133, 6, 135, 8, 137,			\
			  10, 139, 12, 141, 14, 143, 16, 145, 18, 147, 		\
			  20, 149, 22, 151, 24, 153, 26, 155, 28, 157, 30, 159,    \
        32, 161, 34, 163, 36, 165, 38, 167, 40, 169,  \
        42, 171, 44, 173, 46, 175, 48, 177, 50, 179, \
        52, 181, 54, 183, 56, 185, 58, 187, 60, 189, \
        62, 191, \
        64, 193, 66, 195, 68, 197, 70, 199, 72, 201,		\
			  74, 203, 76, 205, 78, 207, 80, 209, 82, 211, 		\
			  84, 213, 86, 215, 88, 217, 90, 219, 92, 221, 94, 223,    \
        96, 225, 98, 227, 100, 229, 102, 231, 104, 233,  \
        106, 235, 108, 237, 110, 239, 112, 241, 114, 243, \
        116, 245, 118, 247, 120, 249, 122, 251, 124, 253, \
        126, 255
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

void __attribute__ ((noipa))
merge0 (vnx128qi x, vnx128qi y, vnx128qi *out)
{
  vnx128qi v = __builtin_shufflevector ((vnx128qi) x, (vnx128qi) y, MASK_128);
  *(vnx128qi*)out = v;
}

void __attribute__ ((noipa))
merge1 (vnx128uqi x, vnx128uqi y, vnx128uqi *out)
{
  vnx128uqi v = __builtin_shufflevector ((vnx128uqi) x, (vnx128uqi) y, MASK_128);
  *(vnx128uqi*)out = v;
}

void __attribute__ ((noipa))
merge2 (vnx64hi x, vnx64hi y, vnx64hi *out)
{
  vnx64hi v = __builtin_shufflevector ((vnx64hi) x, (vnx64hi) y, MASK_64);
  *(vnx64hi*)out = v;
}

void __attribute__ ((noipa))
merge3 (vnx64uhi x, vnx64uhi y, vnx64uhi *out)
{
  vnx64uhi v = __builtin_shufflevector ((vnx64uhi) x, (vnx64uhi) y, MASK_64);
  *(vnx64uhi*)out = v;
}

void __attribute__ ((noipa))
merge4 (vnx32si x, vnx32si y, vnx32si *out)
{
  vnx32si v = __builtin_shufflevector ((vnx32si) x, (vnx32si) y, MASK_32);
  *(vnx32si*)out = v;
}

void __attribute__ ((noipa))
merge5 (vnx32usi x, vnx32usi y, vnx32usi *out)
{
  vnx32usi v = __builtin_shufflevector ((vnx32usi) x, (vnx32usi) y, MASK_32);
  *(vnx32usi*)out = v;
}

void __attribute__ ((noipa))
merge6 (vnx16di x, vnx16di y, vnx16di *out)
{
  vnx16di v = __builtin_shufflevector ((vnx16di) x, (vnx16di) y, MASK_16);
  *(vnx16di*)out = v;
}

void __attribute__ ((noipa))
merge7 (vnx16udi x, vnx16udi y, vnx16udi *out)
{
  vnx16udi v = __builtin_shufflevector ((vnx16udi) x, (vnx16udi) y, MASK_16);
  *(vnx16udi*)out = v;
}

void __attribute__ ((noipa))
merge8 (vnx64hf x, vnx64hf y, vnx64hf *out)
{
  vnx64hf v = __builtin_shufflevector ((vnx64hf) x, (vnx64hf) y, MASK_64);
  *(vnx64hf*)out = v;
}

void __attribute__ ((noipa))
merge9 (vnx32sf x, vnx32sf y, vnx32sf *out)
{
  vnx32sf v = __builtin_shufflevector ((vnx32sf) x, (vnx32sf) y, MASK_32);
  *(vnx32sf*)out = v;
}

void __attribute__ ((noipa))
merge10 (vnx16df x, vnx16df y, vnx16df *out)
{
  vnx16df v = __builtin_shufflevector ((vnx16df) x, (vnx16df) y, MASK_16);
  *(vnx16df*)out = v;
}

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 11 } } */
