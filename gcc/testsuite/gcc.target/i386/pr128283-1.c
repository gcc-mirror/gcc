/* PR target/128283 */
/* { dg-do compile } */
/* { dg-options "-O2 -msgx -fdump-tree-optimized" } */

#include <immintrin.h>
#include <stdint.h>

uint64_t
derive_sgx_key (uint64_t *req_aligned)
{
  uint64_t key[2] __attribute__((aligned(16))) = {0, 0};
  size_t d[3] = {(size_t)req_aligned, (size_t)key, 0};

  _enclu_u32(1 /* EGETKEY */, d);
  return key[0];
}

/* { dg-final { scan-tree-dump " = key\\\[0\\\];" "optimized" } } */
