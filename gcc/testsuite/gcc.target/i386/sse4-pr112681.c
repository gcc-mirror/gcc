/* PR target/112681 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4 -mno-avx" } */

struct S { void *c; char d[16]; } a, b;

int
foo (void)
{
  return __builtin_memcmp (a.d, b.d, sizeof (a.d)) != 0;
}
