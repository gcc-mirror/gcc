/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef _Float16 __v16hf __attribute__ ((__vector_size__ (32)));
typedef _Float16 __m256h __attribute__ ((__vector_size__ (32), __may_alias__));

__m256h
__attribute__ ((noinline, noclone))
foo1 (_Float16 x)
{
  return __extension__ (__m256h)(__v16hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

__m256h
__attribute__ ((noinline, noclone))
foo2 (_Float16 *x)
{
  return __extension__ (__m256h)(__v16hf) { *x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

/* { dg-final { scan-assembler-times "vmovw\[^\n\r]*xmm0" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovw\[^\n\r]*xmm0" 2 { target { ia32 } } } } */
