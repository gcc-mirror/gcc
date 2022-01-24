/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */

/* { dg-final { scan-assembler-times "vpbroadcastw" 1 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "vpblendw" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovsh" 1 { target { ! ia32 } } } }  */

/* { dg-final { scan-assembler-times "vpinsrw" 2 { target ia32 } } }  */

typedef _Float16 __v8hf __attribute__ ((__vector_size__ (16)));
typedef _Float16 __m128h __attribute__ ((__vector_size__ (16), __may_alias__));

__m128h
__attribute__ ((noinline, noclone))
foo1 (__m128h a, _Float16 f)
{
  __v8hf x = (__v8hf) a;
  x[2] = f;
  return (__m128h) x;
}

__m128h
__attribute__ ((noinline, noclone))
foo2 (__m128h a, _Float16 f)
{
  __v8hf x = (__v8hf) a;
  x[0] = f;
  return (__m128h) x;
}
