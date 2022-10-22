/* { dg-do compile } */
/* { dg-options "-O0 -Wno-psabi" } */

typedef int __attribute__((__vector_size__ (16))) V;

static inline __attribute__((__always_inline__)) V
bar (V v128u32_0)
{
  return __builtin_shuffle ((V){}, v128u32_0, v128u32_0);
}

V
foo (void)
{
  return bar ((V){7, 4, 4});
}
