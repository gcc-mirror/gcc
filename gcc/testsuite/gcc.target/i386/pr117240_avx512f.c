/* { dg-do compile } */
/* { dg-options "-O2 -mvaes -mno-xsave -Wno-psabi  -Wno-implicit-function-declaration" } */

typedef __attribute__((__vector_size__(64))) char V;

V
foo(V v)
{
  return __builtin_ia32_vaesenc_v64qi(v, v);/* { dg-error "incompatible types when returning" } */
}
