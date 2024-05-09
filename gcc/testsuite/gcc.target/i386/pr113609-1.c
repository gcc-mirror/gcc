/* PR target/113609 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-not "^cmp" } } */
/* { dg-final { scan-assembler-not "\[ \\t\]+sete" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\[ \\t\]+setne" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\[ \\t\]+je" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\[ \\t\]+jne" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+sete" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+setne" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+je" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+jne" 2 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "kortest" 12 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "kortest" 17 { target { ! ia32 } } } } */

#include <immintrin.h>

unsigned int
cmp_vector_sete_mask8(__m128i a, __m128i b)
{
    __mmask8 k = _mm_cmpeq_epi16_mask (a, b);
    if (k == (__mmask8) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_sete_mask16(__m128i a, __m128i b)
{
    __mmask16 k = _mm_cmpeq_epi8_mask (a, b);
    if (k == (__mmask16) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_sete_mask32(__m256i a, __m256i b)
{
    __mmask32 k = _mm256_cmpeq_epi8_mask (a, b);
    if (k == (__mmask32) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_sete_mask64(__m512i a, __m512i b)
{
    __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
    if (k == (__mmask64) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_setne_mask8(__m128i a, __m128i b)
{
    __mmask8 k = _mm_cmpeq_epi16_mask (a, b);
    if (k != (__mmask8) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_setne_mask16(__m128i a, __m128i b)
{
    __mmask16 k = _mm_cmpeq_epi8_mask (a, b);
    if (k != (__mmask16) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_setne_mask32(__m256i a, __m256i b)
{
    __mmask32 k = _mm256_cmpeq_epi8_mask (a, b);
    if (k != (__mmask32) -1)
      return 1;
    else
      return 0;
}

unsigned int
cmp_vector_setne_mask64(__m512i a, __m512i b)
{
    __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
    if (k != (__mmask64) -1)
      return 1;
    else
      return 0;
}

__m128i
cmp_vector_je_mask8(__m128i a, __m128i b) {
    __mmask8 k = _mm_cmpeq_epi16_mask (a, b);
    if (k == (__mmask8) -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m128i
cmp_vector_je_mask16(__m128i a, __m128i b) {
    __mmask16 k = _mm_cmpeq_epi8_mask (a, b);
    if (k == (__mmask16) -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m256i
cmp_vector_je_mask32(__m256i a, __m256i b) {
    __mmask32 k = _mm256_cmpeq_epi8_mask (a, b);
    if (k == (__mmask32) -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 1;
    }
    return a; 
}

__m512i
cmp_vector_je_mask64(__m512i a, __m512i b) {
    __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
    if (k == (__mmask64) -1) {
	a[0] = a[0] + 1;
    }
    else {
	a[0] = a[0] - 5;
    }
    return a; 
}

__m128i
cmp_vector_jne_mask8(__m128i a, __m128i b) {
    __mmask8 k = _mm_cmpeq_epi16_mask (a, b);
    if (k == (__mmask8) -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m128i
cmp_vector_jne_mask16(__m128i a, __m128i b) {
    __mmask16 k = _mm_cmpeq_epi8_mask (a, b);
    if (k == (__mmask16) -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m256i
cmp_vector_jne_mask32(__m256i a, __m256i b) {
    __mmask32 k = _mm256_cmpeq_epi8_mask (a, b);
    if (k == (__mmask32) -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m512i
cmp_vector_jne_mask64(__m512i a, __m512i b) {
    __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
    if (k == (__mmask64) -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}

__m512i
mask_cmp_vector_jne_mask64(__m512i a, __m512i b) {
    __mmask64 k = _mm512_mask_cmpeq_epi8_mask ((__mmask64)0xffffffefffffffff, a, b);
    if (k == (__mmask64) -1) {
	a[0] = a[0] + 1;
    }
    a[0] = a[0] - 4;
    return a; 
}
