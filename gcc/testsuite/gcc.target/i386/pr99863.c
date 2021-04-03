/* PR rtl-optimization/99863 */
/* { dg-do run } */
/* { dg-options "-O -fno-tree-forwprop -mno-sse2 -Wno-psabi" } */

typedef unsigned char __attribute__((__vector_size__ (8))) A;
typedef unsigned char __attribute__((__vector_size__ (32))) B;
typedef unsigned char __attribute__((__vector_size__ (64))) C;
typedef unsigned int __attribute__((__vector_size__ (32))) D;
typedef unsigned int __attribute__((__vector_size__ (64))) E;
typedef unsigned long long F;

D a;
A b;

A
foo (E x, F y)
{
  D c = (y <= 0) * a;
  x *= (0 < y);
  C d = (C) x;
  B e = ((union { C a; B b[2];}) d).b[0] + (B) c;
  A f = ((union { B a; A b[4];}) e).b[0] + (A) b;
  return f;
}

int
main ()
{
  F x = (F) foo ((E) { 3 }, 5);
  if (x != 3)
    __builtin_abort ();
  return 0;
}
