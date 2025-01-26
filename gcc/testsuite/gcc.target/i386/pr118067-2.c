/* { dg-do compile { target int128 } } */
/* { dg-options "-O -fno-split-wide-types -mavx512f -mtune=k8" } */

typedef unsigned short U __attribute__((__vector_size__(64)));
typedef int V __attribute__((__vector_size__(64)));
typedef __int128 W __attribute__((__vector_size__(64)));

W
foo(U u, V v)
{
  W w;
  /* __asm__ volatile ("" : "=v"(w)); prevents the -Wuninitialized warning */
  u[0] >>= 1;
  v %= (V)w;
  return (W)u + (W)v;
}
