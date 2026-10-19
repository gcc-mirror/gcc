/* { dg-do run { target aarch64_sve2_hw } } */
/* { dg-options "-O3 -march=armv9-a+sve2 -mautovec-preference=sve-only -msve-vector-bits=scalable --param aarch64-vect-unroll-limit=1 --param vect-partial-vector-usage=0" } */

#include <arm_sve.h>
#include <limits.h>
#include <stdlib.h>

#define DEF_SUMS(NAME, INPUT, OUTPUT)                           \
  __attribute__((noipa)) OUTPUT                                \
  NAME (const INPUT *a, unsigned int n)                        \
  {                                                            \
    OUTPUT sum = 0;                                            \
    for (unsigned int i = 0; i < n; ++i)                       \
      sum += a[i];                                             \
    return sum;                                                \
  }                                                            \
                                                               \
  __attribute__((noipa, optimize ("no-tree-vectorize"))) OUTPUT \
  ref_##NAME (const INPUT *a, unsigned int n)                  \
  {                                                            \
    OUTPUT sum = 0;                                            \
    for (unsigned int i = 0; i < n; ++i)                       \
      sum += a[i];                                             \
    return sum;                                                \
  }

DEF_SUMS (sum_s8_s16, signed char, short)
DEF_SUMS (sum_u8_u16, unsigned char, unsigned short)
DEF_SUMS (sum_s16_s32, short, int)
DEF_SUMS (sum_u16_u32, unsigned short, unsigned int)
DEF_SUMS (sum_s32_s64, int, long long)
DEF_SUMS (sum_u32_u64, unsigned int, unsigned long long)

__attribute__((noipa)) unsigned long long
sum_abs_s32_u64 (const int *a, unsigned int n)
{
  unsigned long long sum = 0;
  for (unsigned int i = 0; i < n; ++i)
    sum += __builtin_abs (a[i]);
  return sum;
}

__attribute__((noipa, optimize ("no-tree-vectorize"))) unsigned long long
ref_abs_s32_u64 (const int *a, unsigned int n)
{
  unsigned long long sum = 0;
  for (unsigned int i = 0; i < n; ++i)
    sum += __builtin_abs (a[i]);
  return sum;
}

int
main (void)
{
  unsigned int vl_b = svcntb ();
  unsigned int vl_h = svcnth ();
  unsigned int vl_w = svcntw ();
  unsigned int count = 4 * vl_b + 3;
  unsigned int allocation_count = count + 1;
  signed char *signed_char_values
    = (signed char *) __builtin_alloca (allocation_count
                                        * sizeof (signed char));
  unsigned char *unsigned_char_values
    = (unsigned char *) __builtin_alloca (allocation_count
                                          * sizeof (unsigned char));
  short *signed_short_values
    = (short *) __builtin_alloca (allocation_count * sizeof (short));
  unsigned short *unsigned_short_values
    = (unsigned short *) __builtin_alloca (allocation_count
                                           * sizeof (unsigned short));
  int *signed_int_values
    = (int *) __builtin_alloca (allocation_count * sizeof (int));
  unsigned int *unsigned_int_values
    = (unsigned int *) __builtin_alloca (allocation_count
                                         * sizeof (unsigned int));

  for (unsigned int i = 0; i < allocation_count; ++i)
    {
      static const signed char signed_char_edges[] =
        { SCHAR_MIN, SCHAR_MAX, 1, 0, -1 };
      static const unsigned char unsigned_char_edges[] =
        { 0, 1, UCHAR_MAX, UCHAR_MAX - 1, 0x80 };
      static const short signed_short_edges[] =
        { SHRT_MIN, SHRT_MAX, 1, 0, -1 };
      static const unsigned short unsigned_short_edges[] =
        { 0, 1, USHRT_MAX, USHRT_MAX - 1, 0x8000 };
      static const int signed_int_edges[] =
        { INT_MIN + 1, INT_MAX, -1, 0, 1, -123456789, 123456789 };
      static const unsigned int unsigned_int_edges[] =
        { 0, 1, UINT_MAX, UINT_MAX - 1, 0x80000000U, 123456789U };
      signed_char_values[i] = signed_char_edges[i % 5];
      unsigned_char_values[i] = unsigned_char_edges[i % 5];
      signed_short_values[i] = signed_short_edges[i % 5];
      unsigned_short_values[i] = unsigned_short_edges[i % 5];
      signed_int_values[i] = signed_int_edges[i % 7];
      unsigned_int_values[i] = unsigned_int_edges[i % 6];
    }

  unsigned int lengths[] =
    { 0, 1, vl_w - 1, vl_w, vl_w + 1, vl_h - 1, vl_h, vl_h + 1,
      vl_b - 1, vl_b, vl_b + 1, 2 * vl_b + 1, count };
  for (unsigned int i = 0; i < sizeof (lengths) / sizeof (lengths[0]); ++i)
    for (unsigned int offset = 0; offset <= 1; ++offset)
      {
        unsigned int n = lengths[i];
        if ((sum_s8_s16 (signed_char_values + offset, n)
             != ref_sum_s8_s16 (signed_char_values + offset, n))
            || (sum_u8_u16 (unsigned_char_values + offset, n)
                != ref_sum_u8_u16 (unsigned_char_values + offset, n))
            || (sum_s16_s32 (signed_short_values + offset, n)
                != ref_sum_s16_s32 (signed_short_values + offset, n))
            || (sum_u16_u32 (unsigned_short_values + offset, n)
                != ref_sum_u16_u32 (unsigned_short_values + offset, n))
            || (sum_s32_s64 (signed_int_values + offset, n)
                != ref_sum_s32_s64 (signed_int_values + offset, n))
            || (sum_u32_u64 (unsigned_int_values + offset, n)
                != ref_sum_u32_u64 (unsigned_int_values + offset, n))
            || (sum_abs_s32_u64 (signed_int_values + offset, n)
                != ref_abs_s32_u64 (signed_int_values + offset, n)))
          abort ();
      }
  return 0;
}
