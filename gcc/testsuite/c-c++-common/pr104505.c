/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" } */

typedef char __attribute__((__vector_size__ (8))) U;
typedef short __attribute__((__vector_size__ (16))) V;

U u;

void
foo (V v)
{
  u = __builtin_shufflevector (u, u, __builtin_convertvector (v, U)); /* { dg-error "invalid element index" } */
}
