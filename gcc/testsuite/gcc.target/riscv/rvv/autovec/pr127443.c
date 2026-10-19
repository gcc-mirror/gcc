/* { dg-do compile } */
/* { dg-options "-O -march=rv64gcv -mabi=lp64d -g" } */

typedef __attribute__((__vector_size__(16))) __int128 V;
typedef __attribute__((__vector_size__(16))) char W;

char c;

void
foo(V v)
{
  v &= c;
  c = ((W)v)[1];
}
