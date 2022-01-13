/* { dg-do compile } */
/* { dg-options "-Wno-psabi" } */

typedef int __attribute__((__vector_size__ (sizeof(int)*4))) V;

int
foo(V v, int i)
{
  return __builtin_shufflevector (v, v, 2, 3)[i];
}

int
bar(V v, int i)
{
  return __builtin_shufflevector(v, v, 4)[0] & i;
}
