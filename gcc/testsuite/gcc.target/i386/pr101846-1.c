/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -mavx512dq -O2" } */
/* { dg-final { scan-assembler-not "vmov" } } */
/* { dg-final { scan-assembler-times "vpmovzxbw" "3" } } */
/* { dg-final { scan-assembler-times "vpmovzxwd" "3" } } */
/* { dg-final { scan-assembler-times "vpmovzxdq" "3" } } */

typedef short v4hi __attribute__((vector_size (8)));
typedef short v8hi __attribute__((vector_size (16)));
typedef short v16hi __attribute__((vector_size (32)));
typedef short v32hi __attribute__((vector_size (64)));
typedef char v8qi __attribute__((vector_size (8)));
typedef char v16qi __attribute__((vector_size (16)));
typedef char v32qi __attribute__((vector_size (32)));
typedef char v64qi __attribute__((vector_size (64)));
typedef int v2si __attribute__((vector_size (8)));
typedef int v4si __attribute__((vector_size (16)));
typedef int v8si __attribute__((vector_size (32)));
typedef int v16si __attribute__((vector_size (64)));

v32hi
foo_zxwd_512 (v16hi x)
{
  return __builtin_shufflevector (x, (v16hi) {},
				  0, 16, 1, 17, 2, 18, 3, 19,
				  4, 20, 5, 21, 6, 22, 7, 23,
				  8, 24, 9, 25, 10, 26, 11, 27,
				  12, 28, 13, 29, 14, 30, 15, 31);
}

v16hi
foo_zxwd_256 (v8hi x)
{
  return __builtin_shufflevector (x, (v8hi) {},
				  0, 8, 1, 9, 2, 10, 3, 11,
				  4, 12, 5, 13, 6, 14, 7, 15);
}

v8hi
foo_zxwd_128 (v4hi x)
{
  return __builtin_shufflevector (x, (v4hi) {}, 0, 4, 1, 5, 2, 6, 3, 7);
}

v16si
foo_zxdq_512 (v8si x)
{
  return __builtin_shufflevector (x, (v8si) {},
				  0, 8, 1, 9, 2, 10, 3, 11,
				  4, 12, 5, 13, 6, 14, 7, 15);
}

v8si
foo_zxdq_256 (v4si x)
{
  return __builtin_shufflevector (x, (v4si) {}, 0, 4, 1, 5, 2, 6, 3, 7);
}

v4si
foo_zxdq_128 (v2si x)
{
  return __builtin_shufflevector (x, (v2si) {}, 0, 2, 1, 3);
}

v64qi
foo_zxbw_512 (v32qi x)
{
  return __builtin_shufflevector (x, (v32qi) {},
				  0, 32, 1, 33, 2, 34, 3, 35,
				  4, 36, 5, 37, 6, 38, 7, 39,
				  8, 40, 9, 41, 10, 42, 11, 43,
				  12, 44, 13, 45, 14, 46, 15, 47,
				  16, 48, 17, 49, 18, 50, 19, 51,
				  20, 52, 21, 53, 22, 54, 23, 55,
				  24, 56, 25, 57, 26, 58, 27, 59,
				  28, 60, 29, 61, 30, 62, 31, 63);
}

v32qi
foo_zxbw_256 (v16qi x)
{
  return __builtin_shufflevector (x, (v16qi) {},
				  0, 16, 1, 17, 2, 18, 3, 19,
				  4, 20, 5, 21, 6, 22, 7, 23,
				  8, 24, 9, 25, 10, 26, 11, 27,
				  12, 28, 13, 29, 14, 30, 15, 31);
}

v16qi
foo_zxbw_128 (v8qi x)
{
  return __builtin_shufflevector (x, (v8qi) {},
				  0, 8, 1, 9, 2, 10, 3, 11,
				  4, 12, 5, 13, 6, 14, 7, 15);
}
