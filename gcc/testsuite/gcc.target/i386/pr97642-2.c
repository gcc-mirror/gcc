/* PR target/97642 */
/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -mavx512dq -mavx512vl -mavx512bw" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512dq } */
/* { dg-require-effective-target avx512bw } */

#include <assert.h>
#include <immintrin.h>
#include <stdint.h>
#include <sys/mman.h>

#define N 5

// Faults with GCC because usage of vpblendd
__m256i __attribute__((noinline)) mask_load(uint32_t * arr) {
  __m256i tmp;
  return _mm256_mask_loadu_epi32(tmp, (1 << N) - 1, arr);
}

// Faults
__m256i __attribute__((noinline)) blend_load_asm(uint32_t * arr) {
  __m256i tmp = _mm256_set1_epi64x(0);
  asm volatile("vpblendd %[m], (%[arr]), %[tmp], %[tmp]\n\t"
	       : [ tmp ] "+x"(tmp)
	       : [ arr ] "r"(arr), [ m ] "i"(((1 << N) - 1))
	       :);
  return tmp;
}

// Does not fault
__m256i __attribute__((noinline)) mask_load_asm(uint32_t * arr) {
  __m256i           tmp;
  asm volatile(
	       "movb %[m], %%al\n\t"
	       "kmovb %%eax, %%k1\n\t"
	       "vmovdqu32 (%[arr]), %[tmp] %{%%k1} %{z%}\n\t"
	       : [ tmp ] "+x"(tmp)
	       : [ arr ] "r"(arr), [ m ] "i"(((1 << N) - 1))
	       : "eax", "k1");
  return tmp;
}


void __attribute__((noinline)) mask_store(uint32_t * arr, __m256i v) {
  return _mm256_mask_storeu_epi32(arr, (1 << N) - 1, v);
}


#define NPAGES      (2)
#define END_OF_PAGE (1024 - N)

#ifndef LOAD_METHOD
#define LOAD_METHOD mask_load // mask_load_asm does not fault
#endif


int
main() {
  if (!(__builtin_cpu_supports ("avx512dq")
	&& __builtin_cpu_supports ("avx512vl")
	&& __builtin_cpu_supports ("avx512bw")))
    return 0;

  uint32_t * addr =
    (uint32_t *)mmap(NULL, NPAGES * 4096, PROT_READ | PROT_WRITE,
		     MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

  for (uint32_t i = 0; i < NPAGES; i += 2) {

    uint32_t page_offset      = 1024 * i + END_OF_PAGE;
    uint32_t next_page_offset = 1024 * (i + 1);

    assert(!mprotect(addr + next_page_offset, 4096, PROT_NONE));
    mask_store(addr + page_offset, LOAD_METHOD(addr + page_offset));
  }
}
