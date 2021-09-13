/* PR target/99321 */
/* { dg-do assemble { target lp64 } } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target assembler_march_noavx512bw } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw -Wa,-march=+noavx512bw" } */

#include <x86intrin.h>

typedef unsigned char V1 __attribute__((vector_size (16)));
typedef unsigned char V2 __attribute__((vector_size (32)));
typedef unsigned short V3 __attribute__((vector_size (16)));
typedef unsigned short V4 __attribute__((vector_size (32)));

void f1 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a += b; __asm ("" : : "v" (a)); }
void f2 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a += b; __asm ("" : : "v" (a)); }
void f3 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a += b; __asm ("" : : "v" (a)); }
void f4 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a += b; __asm ("" : : "v" (a)); }
void f5 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a -= b; __asm ("" : : "v" (a)); }
void f6 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a -= b; __asm ("" : : "v" (a)); }
void f7 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a -= b; __asm ("" : : "v" (a)); }
void f8 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a -= b; __asm ("" : : "v" (a)); }
void f9 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a *= b; __asm ("" : : "v" (a)); }
void f10 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a *= b; __asm ("" : : "v" (a)); }
void f11 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_min_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f12 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_min_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f13 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_min_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f14 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_min_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f15 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_min_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f16 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_min_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f17 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_min_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f18 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_min_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f19 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_max_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f20 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_max_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f21 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_max_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f22 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_max_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f23 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_max_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f24 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_max_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f25 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_max_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f26 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_max_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
