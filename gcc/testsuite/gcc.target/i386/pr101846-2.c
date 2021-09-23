/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi -O2" } */
/* { dg-final { scan-assembler-times "vpmovwb" "3" } } */
/* { dg-final { scan-assembler-times "vpmovdw" "3" } } */
/* { dg-final { scan-assembler-times "vpmovqd" "3" } } */

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

v16hi
foo_dw_512 (v32hi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30);
}

v8hi
foo_dw_256 (v16hi x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6, 8, 10, 12, 14);
}

v4hi
foo_dw_128 (v8hi x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6);
}

v8si
foo_qd_512 (v16si x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6, 8, 10, 12, 14);
}

v4si
foo_qd_256 (v8si x)
{
  return __builtin_shufflevector (x, x, 0, 2, 4, 6);
}

v2si
foo_qd_128 (v4si x)
{
  return __builtin_shufflevector (x, x, 0, 2);
}

v32qi
foo_wb_512 (v64qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  32, 34, 36, 38, 40, 42, 44, 46,
				  48, 50, 52, 54, 56, 58, 60, 62);
}

v16qi
foo_wb_256 (v32qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  16, 18, 20, 22, 24, 26, 28, 30);
}

v8qi
foo_wb_128 (v16qi x)
{
  return __builtin_shufflevector (x, x,
				  0, 2, 4, 6, 8, 10, 12, 14);
}
