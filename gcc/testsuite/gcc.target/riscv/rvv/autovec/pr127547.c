/* { dg-do compile } */
/* { dg-options "-O -march=rv64gcv -mabi=lp64d" } */

typedef _Complex __int128 V;
typedef __attribute__((__vector_size__(32))) short W;

_Complex float f;

void
foo(W w)
{
  f *= *(V *)&w;
}
