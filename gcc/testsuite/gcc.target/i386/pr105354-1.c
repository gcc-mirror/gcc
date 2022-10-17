/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-ssse3" } */
/* { dg-final { scan-assembler-times {(?n)psrldq[\t ]+} 16 } } */
/* { dg-final { scan-assembler-times {(?n)pslldq[\t ]+} 16 } } */
/* { dg-final { scan-assembler-times {(?n)por[\t ]+} 16 } } */
/* { dg-final { scan-assembler-times {(?n)pandn[\t ]+} 8 } } */
/* { dg-final { scan-assembler-times {(?n)pand[\t ]+} 8 } } */

typedef short v8hi __attribute__((vector_size (16)));
typedef char v16qi __attribute__((vector_size (16)));

v16qi
__attribute__((noipa))
foo (v16qi a, v16qi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 15, 16, 17, 18, 19, 20);
}

v16qi
__attribute__((noipa))
foo1 (v16qi a, v16qi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 15, 18, 19, 20, 21, 22);
}

v16qi
__attribute__((noipa))
foo2 (v16qi a, v16qi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 16, 17, 18, 19, 20, 21);
}

v16qi
__attribute__((noipa))
foo3 (v16qi a, v16qi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 17, 18, 19, 20, 21, 22);
}

v8hi
__attribute__((noipa))
foo4 (v8hi a, v8hi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 8, 9, 10, 11, 12);
}

v8hi
__attribute__((noipa))
foo5 (v8hi a, v8hi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 7, 9, 10, 11, 12, 13);
}

v8hi
__attribute__((noipa))
foo6 (v8hi a, v8hi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 8, 9, 10, 11, 12, 13);
}

v8hi
__attribute__((noipa))
foo7 (v8hi a, v8hi b)
{
  return __builtin_shufflevector (a, b, 5, 6, 9, 10, 11, 12, 13, 14);
}

v16qi
__attribute__((noipa))
foo8 (v16qi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 15, 16, 17, 18, 19, 20);
}

v16qi
__attribute__((noipa))
foo9 (v16qi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 15, 18, 19, 20, 21, 22);
}

v16qi
__attribute__((noipa))
foo10 (v16qi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 16, 17, 18, 19, 20, 21);
}

v16qi
__attribute__((noipa))
foo11 (v16qi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 8, 9, 10, 11, 12,
				  13, 14, 17, 18, 19, 20, 21, 22);
}

v8hi
__attribute__((noipa))
foo12 (v8hi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 8, 9, 10, 11, 12);
}

v8hi
__attribute__((noipa))
foo13 (v8hi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 7, 9, 10, 11, 12, 13);
}

v8hi
__attribute__((noipa))
foo14 (v8hi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 8, 9, 10, 11, 12, 13);
}

v8hi
__attribute__((noipa))
foo15 (v8hi a)
{
  return __builtin_shufflevector (a, a, 5, 6, 9, 10, 11, 12, 13, 14);
}
