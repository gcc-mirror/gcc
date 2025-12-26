/* { dg-do compile } */
/* { dg-options "-Wno-psabi -O1 -g" } */

typedef unsigned long V __attribute__((__vector_size__(64)));
typedef __int128 U __attribute__((__vector_size__(64)));
U g;

static inline U
bar(V v)
{
  v += ~0;
  v += (V)(U){(unsigned)v[7], 0, 0, 2};
  return (U)v + g;
}

__int128
foo(V v)
{
  return bar(v)[3];
}
