/* PR target/80355 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512vl -mno-avx512dq -mno-avx512bw" } */
/* { dg-final { scan-assembler-times "\tvshufi(?:32x4|64x2)\t" 2 } } */

typedef short V __attribute__((vector_size (64)));
typedef char W __attribute__((vector_size (64)));

W
f0 (W x)
{
  return __builtin_shuffle (x, (W) { 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
				     48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
				     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
				     17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 });
}

V
f1 (V x)
{
  return __builtin_shuffle (x, (V) { 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
				     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 });
}
