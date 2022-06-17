/* { dg-do compile } */
/* { dg-options "-Wno-psabi -O" } */
/* { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } } */
typedef unsigned long long __attribute__((__vector_size__ (16))) U;
typedef unsigned long long __attribute__((__vector_size__ (32))) V;

V
foo (U u)
{
  U x = __builtin_shuffle (u, (U) { 0xBE2ED0AB630B33FE });
  return __builtin_shufflevector (u, x, 2, 1, 0, 3);
}
