/* { dg-do compile } */
/* { dg-options "-O -mcpu=xt-c920 -mrvv-vector-bits=zvl" } */

__attribute__((__vector_size__(sizeof(int)))) int u;
__attribute__((__vector_size__(4 * sizeof(int)))) int v;
__attribute__((__vector_size__(8 * sizeof(int)))) int w;

void
foo()
{
  v ^= __builtin_shufflevector(u, w % 3, 4, 3, 4, 1);
}
