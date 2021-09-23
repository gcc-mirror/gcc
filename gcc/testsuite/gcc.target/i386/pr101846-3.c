/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi -O2" } */
/* { dg-final { scan-assembler-times "vpermb" "2" } } */
/* { dg-final { scan-assembler-times "vpermw" "2" } } */
/* { dg-final { scan-assembler-times "vpermd" "2" } } */

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
foow_512 (v32hi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  16, 17, 18, 19, 20, 21, 22, 23,
				  24, 25, 26, 27, 28, 29, 30, 31);
}

v16hi
foow_256 (v16hi x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6, 8, 10, 12, 14,
				  8, 9, 10, 11, 12, 13, 14, 15);
}


v16si
food_512 (v16si x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6, 8, 10, 12, 14,
				  8, 9, 10, 11, 12, 13, 14, 15);
}

v8si
food_256 (v8si x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6, 4, 5, 6, 7);
}

v64qi
foob_512 (v64qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  32, 34, 36, 38, 40, 42, 44, 46,
				  48, 50, 52, 54, 56, 58, 60, 62,
				  32, 33, 34, 35, 36, 37, 38, 39,
				  40, 41, 42, 43, 44, 45, 46, 47,
				  48, 49, 50, 51, 52, 53, 54, 55,
				  56, 57, 58, 59, 60, 61, 62, 63);
}

v32qi
foob_256 (v32qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  16, 17, 18, 19, 20, 21, 22, 23,
				  24, 25, 26, 27, 28, 29, 30, 31);
}
