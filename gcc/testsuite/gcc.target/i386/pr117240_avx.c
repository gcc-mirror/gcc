/* { dg-do compile } */
/* { dg-options "-O2 -mvaes -mno-xsave -Wno-psabi" } */

typedef __attribute__((__vector_size__(32))) char V;

V
foo(V v)
{
  return __builtin_ia32_vaesenc_v32qi(v, v);/* { dg-error "needs isa option" } */
}
