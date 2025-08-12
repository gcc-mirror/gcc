/* { dg-do compile { target int128 } } */
/* { dg-additional-options "-Wno-psabi" } */

typedef unsigned U __attribute__((__vector_size__(64)));
typedef char V __attribute__((vector_size(64)));
typedef __int128 W __attribute__((vector_size(64)));
char c;
int i;
U u;
V v;
W w;

W
foo()
{
  u = 0 <= u;
  __builtin_mul_overflow(i, c, &u[7]);
  v ^= (V)u;
  return (W)u + w;
}
