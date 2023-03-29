// PR c++/107303
// { dg-options "-Wno-psabi" }

typedef __attribute__((__vector_size__ (2))) unsigned short U;
typedef __attribute__((__vector_size__ (8))) unsigned short V;

U u0, u1, u2;
V v;

void
foo (void)
{
  u0 *= +__builtin_shufflevector (__builtin_shufflevector (u1, v, 3, 1), u2, 0);
}
