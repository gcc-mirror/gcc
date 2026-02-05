/* { dg-do compile } */
/* { dg-additional-options "-O2 --param avoid-fma-max-bits=512 -march=armv9-a -msve-vector-bits=256 -fdump-tree-widening_mul" } */

typedef __attribute__((__vector_size__(32))) char A;
typedef __attribute__((__vector_size__(32))) signed char D;

A c;
char x;

void
foo(D d)
{
  d *= x;
  c += (A)d;
}

/* { dg-final { scan-tree-dump-times "\.FMA" 1 "widening_mul" } } */
