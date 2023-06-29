/* { dg-options "-Wtraditional-conversion -Wno-psabi" } */

typedef int __attribute__((__vector_size__ (4))) V;

void
foo (V v)
{
  foo (v);
}
