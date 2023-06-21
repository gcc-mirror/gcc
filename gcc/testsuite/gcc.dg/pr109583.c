/* PR tree-optimization/109583 */
/* { dg-do compile } */
/* { dg-options "-O1 -Wno-psabi" } */
/* { dg-additional-options "-mno-avx" { target i?86-*-* x86_64-*-* } } */

typedef float v8sf __attribute__((vector_size (8 * sizeof (float))));
typedef int v8si __attribute__((vector_size (8 * sizeof (int))));

#if __SIZEOF_INT__ == __SIZEOF_FLOAT__
v8sf
foo (v8sf x, v8sf y)
{
  v8sf a = x - y;
  v8sf b = x + y;
  return __builtin_shuffle (a, b, (v8si) { 0, 9, 2, 11, 4, 13, 6, 15 });
}

v8sf
bar (v8sf x, v8sf y)
{
  v8sf a = x + y;
  v8sf b = x - y;
  return __builtin_shuffle (a, b, (v8si) { 0, 9, 2, 11, 4, 13, 6, 15 });
}
#endif
