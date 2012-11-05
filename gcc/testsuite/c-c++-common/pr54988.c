/* PR c++/54988 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target { i?86-*-* x86_64-*-* } } } */

#if defined(__i386__) || defined(__x86_64__)
#pragma GCC target "fpmath=sse"
#endif

static inline __attribute__ ((always_inline)) int
foo (int x)
{
  return x;
}

int
bar (int x)
{
  return foo (x);
}
