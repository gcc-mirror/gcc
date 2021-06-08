/* PR target/100887 */
/* { dg-do compile } */
/* { dg-options "-Wno-psabi -w" } */
/* { dg-additional-options "-mavx512f" { target { i?86-*-* x86_64-*-* } } } */

typedef unsigned long long __attribute__((__vector_size__ (2 * sizeof (long long)))) U;
typedef unsigned long long __attribute__((__vector_size__ (4 * sizeof (long long)))) V;
typedef unsigned long long __attribute__((__vector_size__ (8 * sizeof (long long)))) W;

U
foo (V v)
{
  return __builtin_shufflevector ((W){}, v, 0, 8);
}
