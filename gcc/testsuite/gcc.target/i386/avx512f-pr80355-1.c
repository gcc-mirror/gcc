/* PR target/80355 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512vl -mno-avx512dq" } */
/* { dg-final { scan-assembler "\tvshufi32x4\t" } } */
/* { dg-final { scan-assembler "\tvshufi64x2\t" } } */

typedef long long V __attribute__((vector_size (64)));
typedef int W __attribute__((vector_size (64)));

W
f0 (W x)
{
  return __builtin_shuffle (x, (W) { 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7 });
}
V
f1 (V x)
{
  return __builtin_shuffle (x, (V) { 4, 5, 6, 7, 0, 1, 2, 3 });
}
