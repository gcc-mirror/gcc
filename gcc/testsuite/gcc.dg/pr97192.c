/* { dg-do compile } */
/* { dg-options "-O -ftracer" } */
/* { dg-additional-options "-mavx512vl" { target x86_64-*-* i?86-*-* } } */

typedef int __attribute__ ((__vector_size__ (32))) V;

int a, b;
V v;

int
foo (void)
{
  b -= 4 - !a;
  V u = 0 != v == a;
  return u[0];
}
