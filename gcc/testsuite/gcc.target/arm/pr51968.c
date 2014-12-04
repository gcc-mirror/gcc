/* PR target/51968 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-implicit-function-declaration -march=armv7-a -mfloat-abi=softfp -mfpu=neon" } */
/* { dg-require-effective-target arm_neon_ok } */

typedef __builtin_neon_qi int8x8_t __attribute__ ((__vector_size__ (8)));
typedef __builtin_neon_uqi uint8x8_t __attribute__ ((__vector_size__ (8)));
typedef __builtin_neon_qi int8x16_t __attribute__ ((__vector_size__ (16)));
typedef __builtin_neon_hi int16x8_t __attribute__ ((__vector_size__ (16)));
typedef __builtin_neon_si int32x4_t __attribute__ ((__vector_size__ (16)));
struct T { int8x8_t val[2]; };
int y;

void
foo (int8x8_t z, int8x8_t x, int16x8_t b, int8x8_t n)
{
  if (y)
    {
      struct T m;
      __builtin_neon_vuzpv8qi (&m.val[0], z, x);
    }
  for (;;)
    {
      int8x16_t g;
      int8x8_t h, j, k;
      struct T m;
      j = __builtin_neon_vqmovunv8hi (b);
      g = __builtin_neon_vcombinev8qi (j, h);
      k = __builtin_neon_vget_lowv16qi (g);
      __builtin_neon_vuzpv8qi (&m.val[0], k, n);
    }
}
