/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-options "-mavx512vl -mapxf -O2" } */

#include <stdint.h>
#include <immintrin.h>

void broadcast_avx2(int *sx, __m256i *coeff, __m128i *temp) {
  
  __m256i semp[8];


  for (int i = 0; i < 8; i++)
    {
      asm volatile ("" : : : "r8", "r9", "r10", "r11", "r12", "r13",
		    "r14", "r15", "rax", "rcx", "rsi", "rdi", "rdx");
      register volatile uint64_t sm asm ("%r16") = i;
      semp[i] = _mm256_broadcastsi128_si256(temp[sm]);
    }

  coeff[0] = _mm256_unpacklo_epi64(semp[0], semp[1]);
  coeff[1] = _mm256_unpackhi_epi64(semp[2], semp[3]);
  coeff[2] = _mm256_unpacklo_epi64(semp[4], semp[5]);
  coeff[3] = _mm256_unpackhi_epi64(semp[6], semp[7]);
}
