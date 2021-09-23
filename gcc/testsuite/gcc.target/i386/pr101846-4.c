/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi -O2" } */
/* { dg-final { scan-assembler-times "vpermi2b" "3" } } */

typedef char v16qi __attribute__((vector_size (16)));
typedef char v32qi __attribute__((vector_size (32)));
typedef char v64qi __attribute__((vector_size (64)));


v64qi
foob_512 (v64qi x, v64qi y)
{
  return __builtin_shufflevector (x, y,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  32, 34, 36, 38, 40, 42, 44, 46,
				  48, 50, 52, 54, 56, 58, 60, 62,
				  64, 65, 66, 67, 68, 69, 70, 71,
				  72, 73, 74, 77, 79, 74, 72, 70,
				  89, 88, 78, 86, 85, 75, 83, 82,
				  112, 108, 101, 100, 86, 96, 97, 95);
}

v32qi
foob_256 (v32qi x, v32qi y)
{
  return __builtin_shufflevector (x, y,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  32, 34, 36, 38, 40, 42, 44, 46,
				  48, 50, 52, 54, 56, 58, 60, 62);
}

v16qi
foob_128 (v16qi x, v16qi y)
{
  return __builtin_shufflevector (x, y,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30);
}
