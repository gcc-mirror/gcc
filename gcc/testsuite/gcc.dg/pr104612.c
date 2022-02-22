/* PR target/104612 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target i?86-*-* x86_64-*-* } } */

struct V { float x, y; };

struct V
foo (struct V v)
{
  struct V ret;
  ret.x = __builtin_copysignf (1.0e+0, v.x);
  ret.y = __builtin_copysignf (1.0e+0, v.y);
  return ret;
}

float
bar (struct V v)
{
  return __builtin_copysignf (v.x, v.y);
}

float
baz (struct V v)
{
  return v.x * __builtin_copysignf (1.0f, v.y);
}
