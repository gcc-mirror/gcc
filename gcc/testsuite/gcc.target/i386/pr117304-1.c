/* PR target/117304 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-evex512" } */
/* { dg-warning "'-mevex512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

typedef __attribute__((__vector_size__(32))) int __v8si;
typedef __attribute__((__vector_size__(32))) unsigned int __v8su;
typedef __attribute__((__vector_size__(64))) double __v8df;
typedef __attribute__((__vector_size__(64))) int __v16si;
typedef __attribute__((__vector_size__(64))) unsigned int __v16su;
typedef __attribute__((__vector_size__(64))) float __v16sf;
typedef float __m512 __attribute__ ((__vector_size__ (64), __may_alias__));

volatile __v8df df;
volatile __v16sf sf;
volatile __v8si hi;
volatile __v8su hui;
volatile __v16si i;
volatile __v16su ui;

void
foo()
{
  hi ^= __builtin_ia32_cvttpd2dq512_mask(df, hi, 0, 4); /* { dg-error "implicit declaration of function '__builtin_ia32_cvttpd2dq512_mask'; did you mean '__builtin_ia32_\[^\n\r]*'?" } */
  hui ^= __builtin_ia32_cvttpd2udq512_mask(df, hui, 0, 4); /* { dg-error "implicit declaration of function '__builtin_ia32_cvttpd2udq512_mask'; did you mean '__builtin_ia32_\[^\n\r]*'?" } */
  ui ^= __builtin_ia32_cvttps2dq512_mask(sf, ui, 0, 4); /* { dg-error "implicit declaration of function '__builtin_ia32_cvttps2dq512_mask'; did you mean '__builtin_ia32_\[^\n\r]*'?" } */
  ui ^= __builtin_ia32_cvttps2udq512_mask(sf, ui, 0, 4); /* { dg-error "implicit declaration of function '__builtin_ia32_cvttps2udq512_mask'; did you mean '__builtin_ia32_\[^\n\r]*'?" } */
  __builtin_ia32_cvtudq2ps512_mask(ui, sf, 0, 4); /* { dg-error "implicit declaration of function '__builtin_ia32_cvtudq2ps512_mask'; did you mean '__builtin_ia32_\[^\n\r]*'?" } */
}
