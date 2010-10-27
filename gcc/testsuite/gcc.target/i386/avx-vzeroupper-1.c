/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -mtune=generic" } */

#include "avx-check.h"

#ifdef __x86_64__
#define LEN 16
#else
#define LEN 8
#endif

static void
avx_test (void)
{
  __m256i src;

  char reg_save[LEN][32];
  int i, j;

  int s[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  int d[8] = {11, 22, 33, 44, 55, 66, 77, 88};

  __builtin_memset (reg_save, -1, sizeof reg_save);

  src = _mm256_loadu_si256 ((__m256i*) s);

  _mm256_zeroupper (); 

  __asm__ __volatile__ ("vmovdqu %%ymm0,%0":"=m"(reg_save[0]));
  __asm__ __volatile__ ("vmovdqu %%ymm1,%0":"=m"(reg_save[1]));
  __asm__ __volatile__ ("vmovdqu %%ymm2,%0":"=m"(reg_save[2]));
  __asm__ __volatile__ ("vmovdqu %%ymm3,%0":"=m"(reg_save[3]));
  __asm__ __volatile__ ("vmovdqu %%ymm4,%0":"=m"(reg_save[4]));
  __asm__ __volatile__ ("vmovdqu %%ymm5,%0":"=m"(reg_save[5]));
  __asm__ __volatile__ ("vmovdqu %%ymm6,%0":"=m"(reg_save[6]));
  __asm__ __volatile__ ("vmovdqu %%ymm7,%0":"=m"(reg_save[7]));
#ifdef __x86_64__
  __asm__ __volatile__ ("vmovdqu %%ymm8,%0":"=m"(reg_save[8]));
  __asm__ __volatile__ ("vmovdqu %%ymm9,%0":"=m"(reg_save[9]));
  __asm__ __volatile__ ("vmovdqu %%ymm10,%0":"=m"(reg_save[10]));
  __asm__ __volatile__ ("vmovdqu %%ymm11,%0":"=m"(reg_save[11]));
  __asm__ __volatile__ ("vmovdqu %%ymm12,%0":"=m"(reg_save[12]));
  __asm__ __volatile__ ("vmovdqu %%ymm13,%0":"=m"(reg_save[13]));
  __asm__ __volatile__ ("vmovdqu %%ymm14,%0":"=m"(reg_save[14]));
  __asm__ __volatile__ ("vmovdqu %%ymm15,%0":"=m"(reg_save[15]));
#endif

  for (i = 0; i < LEN; i++)
    for (j = 16; j < 32; j++)
      if (reg_save[i][j])
        abort ();

  _mm256_storeu_si256 ((__m256i*) d, src);

}
