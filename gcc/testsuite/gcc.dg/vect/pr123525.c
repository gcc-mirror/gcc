/* { dg-do compile } */
/* { dg-additional-options "-mavx512f" { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-Wno-psabi" } */

typedef __attribute__((__vector_size__(64))) long V;
typedef __attribute__((__vector_size__(64))) int W;

V v;

V
foo(W w)
{
  return w[5] > v;
}
