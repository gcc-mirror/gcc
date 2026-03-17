/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=xt-c920 -mrvv-vector-bits=zvl" } */

typedef __attribute__((__vector_size__(2 * sizeof(int)))) int V;

V
foo(V v)
{
  return v > 0;
}

