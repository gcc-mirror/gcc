/* { dg-do compile } */
/* { dg-options "-mavx2 -mxop -O" } */

typedef char __attribute__((__vector_size__ (16))) V;
typedef unsigned char __attribute__((__vector_size__ (16))) UV;
V v;
UV uv;

V
foo (long c)
{
  return v << c;
}

V
foo1 (long c)
{
  return v >> c;
}

UV
foo2 (unsigned long uc)
{
  return uv >> uc;
}
