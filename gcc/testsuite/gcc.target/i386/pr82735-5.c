/* { dg-do compile { target { lp64 } } }  */
/* { dg-options "-mavx -O2 -mabi=ms -mno-avx512f -masm=att" } */
/* { dg-final { scan-assembler-times {(?n)(?:vmovdqa[1-9]*|vmovap[sd])[\t ]*%xmm[0-9]+, [0-9]*\(%rsp\)} 10 } } */
/* { dg-final { scan-assembler-times {(?n)(?:vmovdqa[1-9]*|vmovap[sd])[\t ]*[0-9]*\(%rsp\), %xmm[0-9]+} 10 } } */

#include <immintrin.h>

void test(char *dest)
{
  __m256i ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7, ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15;
  asm volatile ("vmovdqa\t%%ymm0, %0\n\t"
		"vmovdqa\t%%ymm0, %1\n\t"
		"vmovdqa\t%%ymm0, %2\n\t"
		"vmovdqa\t%%ymm0, %3\n\t"
		"vmovdqa\t%%ymm0, %4\n\t"
		"vmovdqa\t%%ymm0, %5\n\t"
		"vmovdqa\t%%ymm0, %6\n\t"
		"vmovdqa\t%%ymm0, %7\n\t"
		"vmovdqa\t%%ymm0, %8\n\t"
		"vmovdqa\t%%ymm0, %9\n\t"
		"vmovdqa\t%%ymm0, %10\n\t"
		"vmovdqa\t%%ymm0, %11\n\t"
		"vmovdqa\t%%ymm0, %12\n\t"
		"vmovdqa\t%%ymm0, %13\n\t"
		"vmovdqa\t%%ymm0, %14\n\t"
		"vmovdqa\t%%ymm0, %15\n\t"
		: "=v" (ymm1), "=v" (ymm2), "=v"(ymm3), "=v" (ymm4), "=v" (ymm5),
		  "=v" (ymm6), "=v" (ymm7), "=v"(ymm8), "=v" (ymm9), "=v" (ymm10),
		  "=v" (ymm11), "=v" (ymm12), "=v"(ymm13), "=v" (ymm14), "=v" (ymm15),
		  "=v"(ymm0)
		::);
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_storeu_si256((__m256i *)dest, ymm1);
  _mm256_storeu_si256((__m256i *)(dest + 32), ymm2);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 2), ymm3);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 3), ymm4);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 4), ymm5);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 5), ymm6);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 6), ymm7);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 7), ymm8);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 8), ymm9);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 9), ymm10);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 10), ymm11);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 11), ymm12);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 12), ymm13);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 13), ymm14);
  _mm256_storeu_si256((__m256i *)(dest + 32 * 14), ymm15);
}
