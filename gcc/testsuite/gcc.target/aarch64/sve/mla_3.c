/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256 -march=armv8.2-a+sve -fdump-tree-optimized" } */

unsigned int
f1 (unsigned int a, unsigned int b, unsigned int c) {
  unsigned int d = a * b;
  return d + ((c + d) >> 1);
}

unsigned int
g1 (unsigned int a, unsigned int b, unsigned int c) {
  return a * b + c;
}

__Uint32x4_t
f2 (__Uint32x4_t a, __Uint32x4_t b, __Uint32x4_t c) {
  __Uint32x4_t d = a * b;
  return d + ((c + d) >> 1);
}

__Uint32x4_t
g2 (__Uint32x4_t a, __Uint32x4_t b, __Uint32x4_t c) {
  return a * b + c;
}

typedef unsigned int vec __attribute__((vector_size(32))); vec
f3 (vec a, vec b, vec c)
{
  vec d = a * b;
  return d + ((c + d) >> 1);
}

vec
g3 (vec a, vec b, vec c)
{
  return a * b + c;
}

/* { dg-final { scan-tree-dump-times {\.FMA } 1 "optimized" } } */
