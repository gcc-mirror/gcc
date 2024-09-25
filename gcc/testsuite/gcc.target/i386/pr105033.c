/* { dg-do compile } */
/* { dg-options "-march=sapphirerapids -O2" } */
/* { dg-final { scan-assembler-times {vpunpcklqdq[ \t]+} 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {vpunpcklqdq[ \t]+} 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times {vmovhps[ \t]+} 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not {vpermi2[wb][ \t]+} } } */

typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef _Float16 v4hf __attribute__((vector_size (8)));
typedef short v8hi __attribute__((vector_size (16)));
typedef short v4hi __attribute__((vector_size (8)));
typedef char v16qi __attribute__((vector_size (16)));
typedef char v8qi __attribute__((vector_size (8)));

v8hf foo (v4hf a, v4hf b)
{
  return __builtin_shufflevector (a, b, 0, 1, 2, 3, 4, 5, 6, 7);
}

v8hi foo2 (v4hi a, v4hi b)
{
  return __builtin_shufflevector (a, b, 0, 1, 2, 3, 4, 5, 6, 7);
}

v16qi foo3 (v8qi a, v8qi b)
{
  return __builtin_shufflevector (a, b, 0, 1, 2, 3, 4, 5, 6, 7,
				  8, 9, 10, 11, 12, 13, 14, 15);
}
