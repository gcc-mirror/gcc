/* PR target/117116 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */

#include <stdint.h>

#if (defined(__GNUC__) || defined(__clang__))
#include <immintrin.h>
#elif defined(_MSC_VER)
#include <intrin.h>
#endif

typedef struct {
  uint64_t lo64;
  uint64_t mid64;
  uint64_t hi64;
} UInt192;

UInt192 SomeAddFunc(uint64_t a_lo, uint64_t a_hi, uint64_t b) {
  UInt192 result;
  unsigned char cf;
  unsigned long long sum;

  cf = _addcarry_u64(0, a_lo, b, &sum);
  result.lo64 = sum;

  cf = _addcarry_u64(cf, a_hi, 5, &sum);
  result.mid64 = sum;
  result.hi64 = cf;

  return result;
}

/* { dg-final { scan-assembler "adcq\[ \\t\]+\\\$5," } } */

UInt192 SomeSubFunc(uint64_t a_lo, uint64_t a_hi, uint64_t b) {
  UInt192 result;
  unsigned char cf;
  unsigned long long diff;

  cf = _subborrow_u64(0, a_lo, b, &diff);
  result.lo64 = diff;

  cf = _subborrow_u64(cf, a_hi, 17, &diff);
  result.mid64 = diff;
  (void)_subborrow_u64(cf, 0, 0, &diff);
  result.hi64 = diff;

  return result;
}

/* { dg-final { scan-assembler "sbbq\[ \\t\]+\\\$17," } } */
