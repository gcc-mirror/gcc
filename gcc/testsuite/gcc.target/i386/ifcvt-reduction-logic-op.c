/* PR tree-optimization/103126.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx2 -ftree-vectorize -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 3 "vect" } } */
#include<stdint.h>

void xor_bit_arr_nolcd (uint64_t *__restrict mat, uint64_t* a,uint64_t* b, uint64_t *__restrict ans,
    int64_t n)
{
  int64_t i;
  uint64_t vec1, sum1;
  uint64_t vec2, sum2;

  while (n > 0) {
    sum1 = 0;
    vec1 = a[n];
    sum2 = 0;
    vec2 = b[n];

    for (i = 0; i < 64; i++) {
      uint64_t tmp = mat[i];
      uint64_t vec1_i = (vec1 >> i);
      uint64_t vec2_i = (vec2 >> i);
      sum1 ^= (vec1_i & 1) ? tmp : 0;
      if (vec2_i&1) sum2 ^= tmp;
    }
    *ans++ ^= sum1;  n--;
    *ans++ ^= sum2;  n--;
  }
}

void ior_bit_arr_nolcd (uint64_t *__restrict mat, uint64_t* a,uint64_t* b, uint64_t *__restrict ans,
    int64_t n)
{
  int64_t i;
  uint64_t vec1, sum1;
  uint64_t vec2, sum2;

  while (n > 0) {
    sum1 = 0;
    vec1 = a[n];
    sum2 = 0;
    vec2 = b[n];

    for (i = 0; i < 64; i++) {
      uint64_t tmp = mat[i];
      uint64_t vec1_i = (vec1 >> i);
      uint64_t vec2_i = (vec2 >> i);
      sum1 |= (vec1_i & 1) ? tmp : 0;
      if (vec2_i&1) sum2 |= tmp;
    }
    *ans++ |= sum1;  n--;
    *ans++ |= sum2;  n--;
  }
}

void and_bit_arr_nolcd (uint64_t *__restrict mat, uint64_t* a,uint64_t* b, uint64_t *__restrict ans,
    int64_t n)
{
  int64_t i;
  uint64_t vec1, sum1;
  uint64_t vec2, sum2;

  while (n > 0) {
    sum1 = -1;
    vec1 = a[n];
    sum2 = 0;
    vec2 = b[n];

    for (i = 0; i < 64; i++) {
      uint64_t tmp = mat[i];
      uint64_t vec1_i = (vec1 >> i);
      uint64_t vec2_i = (vec2 >> i);
      sum1 &= (vec1_i & 1) ? tmp : -1;
      if (vec2_i&1) sum2 &= tmp;
    }
    *ans++ &= sum1;  n--;
    *ans++ &= sum2;  n--;
  }
}
