/* { dg-do compile } */
/* { dg-options "-Wno-psabi -O2 -fno-tree-ccp -fno-tree-fre -mno-sse" } */

typedef unsigned char U __attribute__((vector_size(16)));
typedef unsigned int V __attribute__((vector_size(16)));
typedef unsigned long long W __attribute__((vector_size(16)));

extern void bar(U, U);

V v;

void
foo(U f)
{
  f[0] = f[0] << (unsigned char)~v[0] | f[~((W)(U){0, 0, 0, 0, 0, 0, 0, 0, 5})[1] & 5] >> (-(unsigned char)~v[0] & 7);
  bar(f, (U){});
}
