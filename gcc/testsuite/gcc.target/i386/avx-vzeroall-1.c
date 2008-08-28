/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static void
avx_test (void)
{
  __m256i src;
#ifdef __x86_64__
  char reg_save[16][32];
  char d[16][32];
#else
  char reg_save[8][32];
  char d[8][32];
#endif  

  int s[8] = {1, 2, 3, 4, 5, 6, 7, 8};

  __builtin_memset (d, 0, sizeof d);
  __builtin_memset (reg_save, -1, sizeof reg_save);

  src = _mm256_loadu_si256 ((__m256i*) s);

  _mm256_zeroall (); 

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

  if (__builtin_memcmp (reg_save, d, sizeof d))
    abort ();

  _mm256_storeu_si256 ((__m256i*) d, src);

}
