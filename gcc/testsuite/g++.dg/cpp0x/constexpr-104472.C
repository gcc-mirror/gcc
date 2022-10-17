// PR c++/104472
// { dg-options "-O2 -frounding-math" }
// { dg-add-options float16 }
// { dg-require-effective-target float16 }

typedef short __attribute__((__vector_size__ (16))) V;
typedef _Float16 __attribute__((__vector_size__ (16))) F;

V v = __builtin_convertvector (__builtin_convertvector ((V){5534}, F), V) < 8;
