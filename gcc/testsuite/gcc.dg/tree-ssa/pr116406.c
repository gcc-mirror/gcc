/* { dg-do compile } */
/* { dg-options "-Os -finstrument-functions-once" } */
/* { dg-additional-options "-mfpmath=387" { target { x86_64-*-* i?86-*-* } } } */

typedef union {
  float f32;
  double f64;
  long i64;
} U;

_Bool
foo (int c, U u)
{
  switch (c)
    {
    case 1:
      return u.f32 - u.f64;
    case 0:
      return u.i64;
    }
}
