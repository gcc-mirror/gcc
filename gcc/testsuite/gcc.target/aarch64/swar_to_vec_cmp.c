/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;

/* 8-bit SWAR tests.  */

static uint8_t packed_cmp_8_8(uint8_t a)
{
  return ((a >> 7) & 0x1U) * 0xffU;
}

/* 16-bit SWAR tests.  */

static uint16_t packed_cmp_8_16(uint16_t a)
{
  return ((a >> 7) & 0x101U) * 0xffU;
}

static uint16_t packed_cmp_16_16(uint16_t a)
{
  return ((a >> 15) & 0x1U) * 0xffffU;
}

/* 32-bit SWAR tests.  */

static uint32_t packed_cmp_8_32(uint32_t a)
{
  return ((a >> 7) & 0x1010101U) * 0xffU;
}

static uint32_t packed_cmp_16_32(uint32_t a)
{
  return ((a >> 15) & 0x10001U) * 0xffffU;
}

static uint32_t packed_cmp_32_32(uint32_t a)
{
  return ((a >> 31) & 0x1U) * 0xffffffffU;
}

/* Driver function to test the vectorized code generated for the different
   packed_cmp variants.  */

#define VECTORIZED_PACKED_CMP(T, FUNC)  	\
  void vectorized_cmp_##FUNC(T* a, int n)	\
  {						\
    n = (n / 32) * 32;				\
    for(int i = 0; i < n; i += 4)		\
    {						\
      a[i + 0] = FUNC(a[i + 0]);		\
      a[i + 1] = FUNC(a[i + 1]);		\
      a[i + 2] = FUNC(a[i + 2]);		\
      a[i + 3] = FUNC(a[i + 3]);		\
    }						\
  }

VECTORIZED_PACKED_CMP(uint8_t, packed_cmp_8_8);

VECTORIZED_PACKED_CMP(uint16_t, packed_cmp_8_16);
VECTORIZED_PACKED_CMP(uint16_t, packed_cmp_16_16);

VECTORIZED_PACKED_CMP(uint32_t, packed_cmp_8_32);
VECTORIZED_PACKED_CMP(uint32_t, packed_cmp_16_32);
VECTORIZED_PACKED_CMP(uint32_t, packed_cmp_32_32);

/* { dg-final { scan-assembler {\tcmlt\t} } } */
/* { dg-final { scan-assembler-not {\tushr\t} } } */
/* { dg-final { scan-assembler-not {\tshl\t} } } */
/* { dg-final { scan-assembler-not {\tmul\t} } } */
