/* { dg-do compile } */
/* { dg-additional-options "-O0" } */
typedef unsigned short __attribute__((__vector_size__ (16))) V;

V
foo (V v)
{
  v /= 255;
  return v;
}
