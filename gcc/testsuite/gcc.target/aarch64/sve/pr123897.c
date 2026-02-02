/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv9-a -msve-vector-bits=256 -fdump-tree-widening_mul" } */

typedef __attribute__((__vector_size__(sizeof(int)*8))) signed int v8i;
typedef __attribute__((__vector_size__(sizeof(int)*8))) unsigned int v8u;
void f(v8i *a,v8i *b,v8u *c)
{
  *c = (v8u)(*a * *b) - *c;
}

void g(v8i *a,v8i *b,v8u *c)
{
  *c = *c - (v8u)(*a * *b);
}

/* { dg-final { scan-tree-dump-times "\.FMA" 2 "widening_mul" } } */
