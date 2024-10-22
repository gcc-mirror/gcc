/* { dg-do compile } */
/* { dg-options "-O2 -mvaes -mevex512 -mno-xsave -Wno-psabi" } */

typedef __attribute__((__vector_size__(64))) char V;

V
foo(V v)
{
  return __builtin_ia32_vaesenc_v64qi(v, v);/* { dg-error "needs isa option" } */
}
