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

void f1 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_abs_epi8 ((__m128i) b); __asm ("" : : "v" (a)); }
void f2 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_abs_epi8 ((__m256i) b); __asm ("" : : "v" (a)); }
void f3 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_abs_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f4 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_abs_epi16 ((__m256i) b); __asm ("" : : "v" (a)); }
void f5 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_adds_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f6 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_adds_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f7 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_adds_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f8 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_adds_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f9 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_subs_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f10 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_subs_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f11 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_subs_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f12 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_subs_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f13 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_adds_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f14 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_adds_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f15 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_adds_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f16 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_adds_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f17 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_subs_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f18 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_subs_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f19 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_subs_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f20 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_subs_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f21 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_alignr_epi8 ((__m128i) a, (__m128i) b, 5); __asm ("" : : "v" (a)); }
void f22 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_alignr_epi8 ((__m256i) a, (__m256i) b, 5); __asm ("" : : "v" (a)); }
void f23 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_adds_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f24 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_avg_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f25 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_avg_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f26 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_avg_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f27 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_avg_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f28 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_broadcastb_epi8 ((__m128i) b); __asm ("" : : "v" (a)); }
void f29 (void) { register V2 a __asm ("%xmm16"); register V1 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_broadcastb_epi8 ((__m128i) b); __asm ("" : : "v" (a)); }
void f30 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_broadcastw_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f31 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_broadcastw_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
int f32 (void) { register V1 a __asm ("%xmm16"); __asm ("" : "=v" (a)); return _mm_extract_epi8 ((__m128i) a, 3); }
int f33 (void) { register V3 a __asm ("%xmm16"); __asm ("" : "=v" (a)); return _mm_extract_epi16 ((__m128i) a, 3); }
void f34 (int c) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_insert_epi8 ((__m128i) b, c, 5); __asm ("" : : "v" (a)); }
void f35 (int c) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_insert_epi16 ((__m128i) b, c, 5); __asm ("" : : "v" (a)); }
void f36 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_maddubs_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f37 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_maddubs_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f38 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_madd_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f39 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_madd_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f40 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_cvtepi8_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f41 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_cvtepi8_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f42 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_cvtepu8_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f43 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_cvtepu8_epi16 ((__m128i) b); __asm ("" : : "v" (a)); }
void f44 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_mulhrs_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f45 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_mulhrs_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f46 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_mulhi_epu16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f47 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_mulhi_epu16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f48 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_mulhi_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f49 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_mulhi_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f50 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_sad_epu8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f51 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_sad_epu8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f52 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_shuffle_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f53 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_shuffle_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f54 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_shufflehi_epi16 ((__m128i) b, 0x5b); __asm ("" : : "v" (a)); }
void f55 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_shufflehi_epi16 ((__m256i) b, 0x5b); __asm ("" : : "v" (a)); }
void f56 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_shufflelo_epi16 ((__m128i) b, 0x5b); __asm ("" : : "v" (a)); }
void f57 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_shufflelo_epi16 ((__m256i) b, 0x5b); __asm ("" : : "v" (a)); }
void f58 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_slli_si128 ((__m128i) b, 3); __asm ("" : : "v" (a)); }
void f59 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_slli_si256 ((__m256i) b, 3); __asm ("" : : "v" (a)); }
void f60 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_srli_si128 ((__m128i) b, 3); __asm ("" : : "v" (a)); }
void f61 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_srli_si256 ((__m256i) b, 3); __asm ("" : : "v" (a)); }
void f62 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_sll_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f63 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_sll_epi16 ((__m256i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f64 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_slli_epi16 ((__m128i) b, 7); __asm ("" : : "v" (a)); }
void f65 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_slli_epi16 ((__m256i) b, 7); __asm ("" : : "v" (a)); }
void f66 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_srl_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f67 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_srl_epi16 ((__m256i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f68 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_srli_epi16 ((__m128i) b, 7); __asm ("" : : "v" (a)); }
void f69 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_srli_epi16 ((__m256i) b, 7); __asm ("" : : "v" (a)); }
void f70 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_sra_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f71 (void) { register V4 a __asm ("%xmm16"); register V3 b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_sra_epi16 ((__m256i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f72 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_srai_epi16 ((__m128i) b, 7); __asm ("" : : "v" (a)); }
void f73 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_srai_epi16 ((__m256i) b, 7); __asm ("" : : "v" (a)); }
void f74 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_unpackhi_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f75 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_unpackhi_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f76 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_unpackhi_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f77 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_unpackhi_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f78 (void) { register V1 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V1) _mm_unpacklo_epi8 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f79 (void) { register V2 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V2) _mm256_unpacklo_epi8 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
void f80 (void) { register V3 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V3) _mm_unpacklo_epi16 ((__m128i) a, (__m128i) b); __asm ("" : : "v" (a)); }
void f81 (void) { register V4 a __asm ("%xmm16"), b __asm ("%xmm17"); __asm ("" : "=v" (a), "=v" (b)); a = (V4) _mm256_unpacklo_epi16 ((__m256i) a, (__m256i) b); __asm ("" : : "v" (a)); }
