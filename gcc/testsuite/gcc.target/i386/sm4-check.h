#include <stdlib.h>
#include "m256-check.h"

static void sm4_test (void);

typedef union
{
  unsigned int x;
  unsigned char a[4];
} union32ui_ub;

unsigned char sbox[256] = {
0xD6, 0x90, 0xE9, 0xFE, 0xCC, 0xE1, 0x3D, 0xB7,
0x16, 0xB6, 0x14, 0xC2, 0x28, 0xFB, 0x2C, 0x05,
0x2B, 0x67, 0x9A, 0x76, 0x2A, 0xBE, 0x04, 0xC3,
0xAA, 0x44, 0x13, 0x26, 0x49, 0x86, 0x06, 0x99,
0x9C, 0x42, 0x50, 0xF4, 0x91, 0xEF, 0x98, 0x7A,
0x33, 0x54, 0x0B, 0x43, 0xED, 0xCF, 0xAC, 0x62,
0xE4, 0xB3, 0x1C, 0xA9, 0xC9, 0x08, 0xE8, 0x95,
0x80, 0xDF, 0x94, 0xFA, 0x75, 0x8F, 0x3F, 0xA6,
0x47, 0x07, 0xA7, 0xFC, 0xF3, 0x73, 0x17, 0xBA,
0x83, 0x59, 0x3C, 0x19, 0xE6, 0x85, 0x4F, 0xA8,
0x68, 0x6B, 0x81, 0xB2, 0x71, 0x64, 0xDA, 0x8B,
0xF8, 0xEB, 0x0F, 0x4B, 0x70, 0x56, 0x9D, 0x35,
0x1E, 0x24, 0x0E, 0x5E, 0x63, 0x58, 0xD1, 0xA2,
0x25, 0x22, 0x7C, 0x3B, 0x01, 0x21, 0x78, 0x87,
0xD4, 0x00, 0x46, 0x57, 0x9F, 0xD3, 0x27, 0x52,
0x4C, 0x36, 0x02, 0xE7, 0xA0, 0xC4, 0xC8, 0x9E,
0xEA, 0xBF, 0x8A, 0xD2, 0x40, 0xC7, 0x38, 0xB5,
0xA3, 0xF7, 0xF2, 0xCE, 0xF9, 0x61, 0x15, 0xA1,
0xE0, 0xAE, 0x5D, 0xA4, 0x9B, 0x34, 0x1A, 0x55,
0xAD, 0x93, 0x32, 0x30, 0xF5, 0x8C, 0xB1, 0xE3,
0x1D, 0xF6, 0xE2, 0x2E, 0x82, 0x66, 0xCA, 0x60,
0xC0, 0x29, 0x23, 0xAB, 0x0D, 0x53, 0x4E, 0x6F,
0xD5, 0xDB, 0x37, 0x45, 0xDE, 0xFD, 0x8E, 0x2F,
0x03, 0xFF, 0x6A, 0x72, 0x6D, 0x6C, 0x5B, 0x51,
0x8D, 0x1B, 0xAF, 0x92, 0xBB, 0xDD, 0xBC, 0x7F,
0x11, 0xD9, 0x5C, 0x41, 0x1F, 0x10, 0x5A, 0xD8,
0x0A, 0xC1, 0x31, 0x88, 0xA5, 0xCD, 0x7B, 0xBD,
0x2D, 0x74, 0xD0, 0x12, 0xB8, 0xE5, 0xB4, 0xB0,
0x89, 0x69, 0x97, 0x4A, 0x0C, 0x96, 0x77, 0x7E,
0x65, 0xB9, 0xF1, 0x09, 0xC5, 0x6E, 0xC6, 0x84,
0x18, 0xF0, 0x7D, 0xEC, 0x3A, 0xDC, 0x4D, 0x20,
0x79, 0xEE, 0x5F, 0x3E, 0xD7, 0xCB, 0x39, 0x48
};

static unsigned
rol32 (unsigned w, int n)
{
  int count = n % 32;
  return ((w << count) | (w >> (32 - count)));
}

static unsigned char
sbox_byte (unsigned w, int i)
{
  union32ui_ub tmp;
  tmp.x = w;
  return sbox[tmp.a[i]];
}

static unsigned
lower_t (unsigned w)
{
  union32ui_ub tmp;
  tmp.a[0] = sbox_byte (w, 0);
  tmp.a[1] = sbox_byte (w, 1);
  tmp.a[2] = sbox_byte (w, 2);
  tmp.a[3] = sbox_byte (w, 3);
  return tmp.x;
}

static unsigned
l_key (unsigned w)
{
  return w ^ rol32 (w, 13) ^ rol32 (w, 23);
}

static unsigned
l_rnds (unsigned w)
{
  unsigned tmp = w;
  tmp = tmp ^ rol32 (w, 2);
  tmp = tmp ^ rol32 (w, 10);
  tmp = tmp ^ rol32 (w, 18);
  tmp = tmp ^ rol32 (w, 24);
  return tmp;
}

#define SM4_FUNC(name)					      \
static unsigned						      \
t_##name (unsigned w)					      \
{							      \
  return l_##name (lower_t (w));			      \
}							      \
							      \
static unsigned						      \
f_##name (unsigned x0, unsigned x1, unsigned x2, unsigned x3, unsigned k) \
{							      \
  return x0 ^ t_##name (x1 ^ x2 ^ x3 ^ k);		      \
}							      \
							      \
static void						      \
compute_sm4##name##4 (int *dst, int *src1, int *src2, int vl) \
{							      \
  unsigned c[4], p[4];					      \
							      \
  int kl = vl / 128;					      \
  int i;						      \
							      \
  for (i = 0; i < kl; i++)				      \
  {							      \
    p[0] = src1[4 * i];					      \
    p[1] = src1[4 * i + 1];				      \
    p[2] = src1[4 * i + 2];				      \
    p[3] = src1[4 * i + 3];				      \
							      \
    c[0] = f_##name (p[0], p[1], p[2], p[3], src2[4 * i]);    \
    c[1] = f_##name (p[1], p[2], p[3], c[0], src2[4 * i + 1]);	\
    c[2] = f_##name (p[2], p[3], c[0], c[1], src2[4 * i + 2]);	\
    c[3] = f_##name (p[3], c[0], c[1], c[2], src2[4 * i + 3]);	\
							      \
    dst[4 * i] = c[0];					      \
    dst[4 * i + 1] = c[1];				      \
    dst[4 * i + 2] = c[2];				      \
    dst[4 * i + 3] = c[3];				      \
  }							      \
}

#define SM4_AVX_SIMULATE(name)				      \
  union128i_d src1, src2, res1;				      \
  int dst1[4] = {0, 0, 0, 0};				      \
							      \
  src1.x = _mm_set_epi32 (111, 222, 333, 444);		      \
  src2.x = _mm_set_epi32 (555, 666, 777, 888);		      \
  res1.x = _mm_set_epi32 (0, 0, 0, 0);			      \
							      \
  res1.x = _mm_sm4##name##4_epi32 (src1.x, src2.x);	      \
							      \
  compute_sm4##name##4 (dst1, src1.a, src2.a, 128);	      \
							      \
  if (check_union128i_d (res1, dst1))			      \
    abort ();						      \
							      \
  union256i_d src3, src4, res2;				      \
  int dst2[8] = {0, 0, 0, 0, 0, 0, 0, 0};		      \
							      \
  src3.x = _mm256_set_epi32 (111, 222, 333, 444, 555, 666, 777, 888); \
  src4.x = _mm256_set_epi32 (999, 123, 456, 789, 135, 792, 468, 147); \
  res2.x = _mm256_set_epi32 (0, 0, 0, 0, 0, 0, 0, 0);	      \
							      \
  res2.x = _mm256_sm4##name##4_epi32 (src3.x, src4.x);	      \
							      \
  compute_sm4##name##4 (dst2, src3.a, src4.a, 256);	      \
							      \
  if (check_union256i_d (res2, dst2))			      \
    abort ();

static void
__attribute__ ((noinline))
do_test (void)
{
  sm4_test ();
}

int
main ()
{
  /* Check CPU support for SM4.  */
  if (__builtin_cpu_supports ("sm4"))
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
      return 0;
    }

#ifdef DEBUG
  printf ("SKIPPED\n");
#endif
  return 0;
}
