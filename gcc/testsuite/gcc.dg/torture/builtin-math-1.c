/* Copyright (C) 2002, 2003, 2004  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.

   Written by Roger Sayle, 16th August 2002.  */

/* { dg-do link } */

extern double atan (double);
extern float atanf (float);
extern long double atanl (long double);
extern double cbrt (double);
extern float cbrtf (float);
extern long double cbrtl (long double);
extern double cos (double);
extern float cosf (float);
extern long double cosl (long double);
extern double exp (double);
extern float expf (float);
extern long double expl (long double);
extern double log (double);
extern float logf (float);
extern long double logl (long double);
extern double pow (double, double);
extern float powf (float, float);
extern long double powl (long double, long double);
extern double sin (double);
extern float sinf (float);
extern long double sinl (long double);
extern double sqrt (double);
extern float sqrtf (float);
extern long double sqrtl (long double);
extern double tan (double);
extern float tanf (float);
extern long double tanl (long double);

/* All references to link_error should go away at compile-time.  */
extern void link_error(void);

void test (float f, double d, long double ld)
{
  if (sqrt (0.0) != 0.0)
    link_error ();

  if (sqrt (1.0) != 1.0)
    link_error ();

  if (cbrt (0.0) != 0.0)
    link_error ();

  if (cbrt (1.0) != 1.0)
    link_error ();

  if (cbrt (-1.0) != -1.0)
    link_error ();

  if (exp (0.0) != 1.0)
    link_error ();

  if (exp (1.0) <= 2.71 || exp (1.0) >= 2.72)
    link_error ();

  if (log (1.0) != 0.0)
    link_error ();

  if (sin (0.0) != 0.0)
    link_error ();

  if (cos (0.0) != 1.0)
    link_error ();

  if (tan (0.0) != 0.0)
    link_error ();

  if (atan (0.0) != 0.0)
    link_error ();

  if (4.0*atan (1.0) <= 3.14 || 4.0*atan (1.0) >= 3.15)
    link_error ();

  if (pow (d, 0.0) != 1.0)
    link_error ();

  if (pow (1.0, d) != 1.0)
    link_error ();


  if (sqrtf (0.0F) != 0.0F)
    link_error ();

  if (sqrtf (1.0F) != 1.0F)
    link_error ();

  if (cbrtf (0.0F) != 0.0F)
    link_error ();

  if (cbrtf (1.0F) != 1.0F)
    link_error ();

  if (cbrtf (-1.0F) != -1.0F)
    link_error ();

  if (expf (0.0F) != 1.0F)
    link_error ();

  if (expf (1.0F) <= 2.71F || expf (1.0F) >= 2.72F)
    link_error ();

  if (logf (1.0F) != 0.0F)
    link_error ();

  if (sinf (0.0F) != 0.0F)
    link_error ();

  if (cosf (0.0F) != 1.0F)
    link_error ();

  if (tanf (0.0F) != 0.0F)
    link_error ();

  if (atanf (0.0F) != 0.0F)
    link_error ();

  if (4.0F*atanf (1.0F) <= 3.14F || 4.0F*atanf (1.0F) >= 3.15F)
    link_error ();

  if (powf (f, 0.0F) != 1.0F)
    link_error ();

  if (powf (1.0F, f) != 1.0F)
    link_error ();


  if (sqrtl (0.0L) != 0.0L)
    link_error ();

  if (sqrtl (1.0L) != 1.0L)
    link_error ();

  if (cbrtl (0.0L) != 0.0L)
    link_error ();

  if (cbrtl (1.0L) != 1.0L)
    link_error ();

  if (cbrtl (-1.0L) != -1.0L)
    link_error ();

  if (expl (0.0L) != 1.0L)
    link_error ();

  if (expl (1.0L) <= 2.71L || expl (1.0L) >= 2.72L)
    link_error ();

  if (logl (1.0L) != 0.0L)
    link_error ();

  if (sinl (0.0L) != 0.0L)
    link_error ();

  if (cosl (0.0L) != 1.0L)
    link_error ();

  if (tanl (0.0L) != 0.0L)
    link_error ();

  if (atanl (0.0) != 0.0L)
    link_error ();

  if (4.0L*atanl (1.0L) <= 3.14L || 4.0L*atanl (1.0L) >= 3.15L)
    link_error ();

  if (powl (ld, 0.0L) != 1.0L)
    link_error ();

  if (powl (1.0L, ld) != 1.0L)
    link_error ();
}

int main()
{
  test (3.0, 3.0F, 3.0L);

  return 0;
}
