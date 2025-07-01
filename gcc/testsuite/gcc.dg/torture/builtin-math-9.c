/* Copyright (C) 1988-2025 Free Software Foundation, Inc.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.  */

/* { dg-do link } */
/* { dg-require-effective-target foldable_pi_based_trigonometry } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

extern double acospi (double);
extern double asinpi (double);
extern double atanpi (double);
extern double atan2pi (double, double);
extern double cospi (double);
extern double sinpi (double);
extern double tanpi (double);

/* All references to link_error should go away at compile-time.  */
extern void link_error (void);

void
test_normal ()
{
  if (acospi (0.5) < 0.3333 || acospi (0.5) > 0.3334 || acospi (-0.5) < 0.6666
      || acospi (-0.5) > 0.6667)
    link_error ();

  if (asinpi (0.5) < 0.1666 || asinpi (0.5) > 0.1667 || asinpi (-0.5) < -0.1667
      || asinpi (-0.5) > -0.1666)
    link_error ();

  if (atanpi (1.0) != 0.25 || atanpi (-1.0) > -0.25)
    link_error ();

  if (atan2pi (1.0, 1.0) > 0.2501 || atan2pi (1.0, 1.0) < 0.2499
      || atan2pi (1.0, -1.0) < 0.7499 || atan2pi (1.0, -1.0) > 0.7501)
    link_error ();

  if (cospi (1.0 / 3) > 0.5001 || cospi (-1.0 / 3) < 0.4999
      || cospi (4.0 / 3) > -0.4999 || cospi (4.0 / 3) < -0.5001)
    link_error ();

  if (sinpi (1.0 / 6) > 0.5001 || sinpi (-1.0 / 6) > -0.4999
      || sinpi (5.0 / 6) < 0.4999 || sinpi (-7.0 / 6) > 0.5001)
    link_error ();

  if (tanpi (0.25) != 1.0000 || tanpi (-0.25) != -1.0000
      || tanpi (1.25) != 1.0000 || tanpi (-1.25) != -1.0000)
    link_error ();
}

void
test_corner ()
{
  if (__builtin_signbit (acospi (1.0)))
    link_error ();

  if (__builtin_signbit (asinpi (0.0)) || !__builtin_signbit (asinpi (-0.0)))
    link_error ();

  if (__builtin_signbit (atanpi (0.0)) || !__builtin_signbit (atanpi (-0.0)))
    link_error ();

  if (__builtin_signbit (atan2pi (0.0, 0.0))
      || !__builtin_signbit (atan2pi (-0.0, 0.0)) || atan2pi (0.0, -0.0) != 1
      || atan2pi (-0.0, -0.0) != -1)
    link_error ();

  if (__builtin_signbit (cospi (0.5)) || __builtin_signbit (cospi (-0.5)))
    link_error ();

  if (__builtin_signbit (sinpi (1)) || !__builtin_signbit (sinpi (-1)))
    link_error ();

  if (__builtin_signbit (tanpi (2)) || __builtin_signbit (tanpi (-3))
      || !__builtin_signbit (tanpi (5)) || !__builtin_signbit (tanpi (-6)))
    link_error ();
}

int
main ()
{
  test_normal ();
  test_corner ();
  return 0;
}
