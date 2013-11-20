/* Test complex divide does not have the bug identified in N1496.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */
/* { dg-add-options ieee } */

extern void abort (void);
extern void exit (int);

#define CMPLX(x, y) __builtin_complex ((double) (x), (double) (y))
#define CMPLXF(x, y) __builtin_complex ((float) (x), (float) (y))
#define CMPLXL(x, y) __builtin_complex ((long double) (x), (long double) (y))
#define NAN __builtin_nanf ("")
#define isnan(x) __builtin_isnan (x)

volatile _Complex float num_f = CMPLXF (1, 1);
volatile _Complex float den_f = CMPLXF (0, NAN);
volatile _Complex float res_f, cres_f = CMPLXF (1, 1) / CMPLXF (0, NAN);

volatile _Complex double num_d = CMPLX (1, 1);
volatile _Complex double den_d = CMPLX (0, NAN);
volatile _Complex double res_d, cres_d = CMPLX (1, 1) / CMPLX (0, NAN);

volatile _Complex long double num_ld = CMPLXL (1, 1);
volatile _Complex long double den_ld = CMPLXL (0, NAN);
volatile _Complex long double res_ld, cres_ld = CMPLXL (1, 1) / CMPLXL (0, NAN);

int
main (void)
{
  res_f = num_f / den_f;
  if (!isnan (__real__ res_f) || !isnan (__imag__ res_f)
      || !isnan (__real__ cres_f) || !isnan (__imag__ cres_f))
    abort ();
  res_d = num_d / den_d;
  if (!isnan (__real__ res_d) || !isnan (__imag__ res_d)
      || !isnan (__real__ cres_d) || !isnan (__imag__ cres_d))
    abort ();
  res_ld = num_ld / den_ld;
  if (!isnan (__real__ res_ld) || !isnan (__imag__ res_ld)
      || !isnan (__real__ cres_ld) || !isnan (__imag__ cres_ld))
    abort ();
  exit (0);
}
