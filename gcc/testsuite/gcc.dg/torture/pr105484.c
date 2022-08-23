/* { dg-do compile } */
/* { dg-additional-options "-fnon-call-exceptions -fno-tree-dce -fno-tree-forwprop" } */
/* { dg-additional-options "-march=cannonlake" { target x86_64-*-* i?86-*-* } } */

typedef int __attribute__((__vector_size__ (16))) V;

void bar (int i);

void
foo (int i)
{
  V v;
  __builtin_mul_overflow (7, i, &v[i]);
  bar ((V){}[3]);
}
