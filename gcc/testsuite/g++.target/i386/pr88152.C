// PR target/88152
// { dg-do compile }
// { dg-options "-O2 -mavx2 -std=c++11" }
// { dg-final { scan-assembler-times "vpmovmskb\[^\n\r]*xmm" 6 } }
// { dg-final { scan-assembler-times "vpmovmskb\[^\n\r]*ymm" 6 } }
// { dg-final { scan-assembler-times "vmovmskps\[^\n\r]*xmm" 4 } }
// { dg-final { scan-assembler-times "vmovmskps\[^\n\r]*ymm" 4 } }
// { dg-final { scan-assembler-times "vmovmskpd\[^\n\r]*xmm" 4 } }
// { dg-final { scan-assembler-times "vmovmskpd\[^\n\r]*ymm" 4 } }
// { dg-final { scan-assembler-not "vpcmpgt|vpcmpeq|vpsra" } }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <x86intrin.h>

template <typename T, size_t N>
using V [[gnu::vector_size(N)]] = T;

int f0 (V<unsigned char, 16> a) { return _mm_movemask_epi8 (reinterpret_cast<__m128i> (a > 0x7f)); }
long int f1 (V<unsigned char, 16> a) { return (unsigned) _mm_movemask_epi8 (reinterpret_cast<__m128i> (a >= 0x80)); }
long int f2 (V<signed char, 16> a) { return (unsigned) _mm_movemask_epi8 (reinterpret_cast<__m128i> (a < 0)); }
int f3 (V<signed char, 16> a) { return _mm_movemask_epi8 (reinterpret_cast<__m128i> (a <= -1)); }
int f4 (V<char, 16> a) { return _mm_movemask_epi8 (reinterpret_cast<__m128i> (a < 0)); }
long int f5 (V<char, 16> a) { return (unsigned) _mm_movemask_epi8 (reinterpret_cast<__m128i> (a <= -1)); }
int f6 (V<unsigned int, 16> a) { return _mm_movemask_ps (reinterpret_cast<__m128> (a > __INT_MAX__)); }
int f7 (V<unsigned int, 16> a) { return _mm_movemask_ps (reinterpret_cast<__m128> (a >= 1U + __INT_MAX__)); }
int f8 (V<int, 16> a) { return _mm_movemask_ps (reinterpret_cast<__m128> (a < 0)); }
int f9 (V<int, 16> a) { return _mm_movemask_ps (reinterpret_cast<__m128> (a <= -1)); }
int f10 (V<unsigned long long, 16> a) { return _mm_movemask_pd (reinterpret_cast<__m128d> (a > __LONG_LONG_MAX__)); }
int f11 (V<unsigned long long, 16> a) { return _mm_movemask_pd (reinterpret_cast<__m128d> (a >= 1ULL + __LONG_LONG_MAX__)); }
long int f12 (V<long long, 16> a) { return (unsigned) _mm_movemask_pd (reinterpret_cast<__m128d> (a < 0)); }
int f13 (V<long long, 16> a) { return _mm_movemask_pd (reinterpret_cast<__m128d> (a <= -1)); }
int f14 (V<unsigned char, 32> a) { return _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a > 0x7f)); }
int f15 (V<unsigned char, 32> a) { return _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a >= 0x80)); }
long int f16 (V<signed char, 32> a) { return (unsigned) _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a < 0)); }
int f17 (V<signed char, 32> a) { return _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a <= -1)); }
int f18 (V<char, 32> a) { return _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a < 0)); }
int f19 (V<char, 32> a) { return _mm256_movemask_epi8 (reinterpret_cast<__m256i> (a <= -1)); }
long int f20 (V<unsigned int, 32> a) { return (unsigned) _mm256_movemask_ps (reinterpret_cast<__m256> (a > __INT_MAX__)); }
int f21 (V<unsigned int, 32> a) { return _mm256_movemask_ps (reinterpret_cast<__m256> (a >= 1U + __INT_MAX__)); }
int f22 (V<int, 32> a) { return _mm256_movemask_ps (reinterpret_cast<__m256> (a < 0)); }
int f23 (V<int, 32> a) { return _mm256_movemask_ps (reinterpret_cast<__m256> (a <= -1)); }
int f24 (V<unsigned long long, 32> a) { return _mm256_movemask_pd (reinterpret_cast<__m256d> (a > __LONG_LONG_MAX__)); }
int f25 (V<unsigned long long, 32> a) { return _mm256_movemask_pd (reinterpret_cast<__m256d> (a >= 1ULL + __LONG_LONG_MAX__)); }
int f26 (V<long long, 32> a) { return _mm256_movemask_pd (reinterpret_cast<__m256d> (a < 0)); }
long int f27 (V<long long, 32> a) { return (unsigned) _mm256_movemask_pd (reinterpret_cast<__m256d> (a <= -1)); }
