/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef _Float16 __v32hf __attribute__ ((__vector_size__ (64)));
typedef _Float16 __m512h __attribute__ ((__vector_size__ (64), __may_alias__));

__m512h
__attribute__ ((noinline, noclone))
foo1 (_Float16 x)
{
  return __extension__ (__m512h)(__v32hf) { x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

__m512h
__attribute__ ((noinline, noclone))
foo2 (_Float16 *x)
{
  return __extension__ (__m512h)(__v32hf) { *x, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f,
                                            0.0f, 0.0f, 0.0f, 0.0f };
}

/* { dg-final { scan-assembler-times "vmovw\[^\n\r]*xmm0" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovw\[^\n\r]*xmm0" 2 { target { ia32 } } } } */
