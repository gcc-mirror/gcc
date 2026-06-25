/* PR target/125949 */
/* On a target with AVX-VNNI-INT8 the vpdpb{ss,su,uu}d[s] instructions only
   have a VEX encoding.  Even under register pressure the compiler must stay
   within xmm/ymm0-15 for these insns and must not allocate xmm/ymm16-31 and
   emit an EVEX-encoded vpdpb* using a high register.  -mavx512vl exposes the
   xmm/ymm16-31 register file so the allocator would otherwise be tempted to
   use it.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavxvnniint8 -mavx512vl" } */
/* { dg-final { scan-assembler-not "vpdpb\[a-z\]+\[ \\t\]+\[^\n\]*%\[xy\]mm(1\[6-9\]|2\[0-9\]|3\[01\])" } } */

#include <immintrin.h>

#define NACC 32

void
foo (unsigned int *restrict out, const unsigned char *restrict a,
     const unsigned char *restrict b, int n)
{
  __m256i acc[NACC];

  for (int k = 0; k < NACC; k++)
    acc[k] = _mm256_setzero_si256 ();

  for (int i = 0; i < n; i++)
    {
      __m256i vb = _mm256_loadu_si256 ((const __m256i *) (b + 32 * (i & 7)));
      for (int k = 0; k < NACC; k++)
	{
	  __m256i va
	    = _mm256_loadu_si256 ((const __m256i *) (a + 32 * ((i + k) & 15)));
	  acc[k] = _mm256_dpbuud_epi32 (acc[k], va, vb);
	}
    }

  for (int k = 0; k < NACC; k++)
    _mm256_storeu_si256 ((__m256i *) (out + 8 * k), acc[k]);
}
