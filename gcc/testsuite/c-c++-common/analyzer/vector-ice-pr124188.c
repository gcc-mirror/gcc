/* { dg-additional-options "-Wno-psabi" } */

typedef __attribute__((__vector_size__(64))) char V;

V g;

void
foo(V a)
{
  V v = -a;
  a + v + g;
}
