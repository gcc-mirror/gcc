// PR c++/105871
// { dg-do compile }
// { dg-options "-Wno-psabi" }

typedef __attribute__((__vector_size__ ( 1))) unsigned char U;
typedef __attribute__((__vector_size__ (16))) unsigned char V;

U
foo (void)
{
  return __builtin_shufflevector ((U){}, (V){}, 0);
}
