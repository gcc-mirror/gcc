/* { dg-do compile } */
/* { dg-options "-O3 -march=armv9-a+sve2 -mautovec-preference=sve-only -msve-vector-bits=scalable --param aarch64-vect-unroll-limit=1 --param vect-partial-vector-usage=0" } */

#define DEF(NAME, INPUT, OUTPUT)                 \
  OUTPUT                                        \
  NAME (const INPUT *a, unsigned int n)         \
  {                                             \
    OUTPUT sum = 0;                             \
    for (unsigned int i = 0; i < n; ++i)        \
      sum += a[i];                              \
    return sum;                                 \
  }

DEF (sum_s8_s16, signed char, short)
DEF (sum_u8_u16, unsigned char, unsigned short)
DEF (sum_s16_s32, short, int)
DEF (sum_u16_u32, unsigned short, unsigned int)
DEF (sum_s32_s64, int, long long)
DEF (sum_u32_u64, unsigned int, unsigned long long)

unsigned long long
sum_abs_s32_u64 (const int *a, unsigned int n)
{
  unsigned long long sum = 0;
  for (unsigned int i = 0; i < n; ++i)
    sum += __builtin_abs (a[i]);
  return sum;
}

/* { dg-final { scan-assembler-times {\tsadalp\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsaddw[bt]\t} } } */
/* { dg-final { scan-assembler-not {\tuaddw[bt]\t} } } */
