/* Test for diagnostics for Wconversion for floating-point.  */

/* { dg-do compile } */
/* { dg-skip-if "doubles are floats" { "avr-*-*" } { "*" } { "" } } */
/* { dg-options "-std=c99 -Wconversion" { target c } } */
/* { dg-options "-Wconversion" { target c++ } } */
/* { dg-require-effective-target large_double } */

float  vfloat;
double vdouble;
long double vlongdouble;

void ffloat (float f);
void fdouble (double d);
void flongdouble (long double ld);

void h (void)
{
  float f = 0;
  double d = 0;
  long double ld = 0;

  ffloat (3.1); /* { dg-warning "conversion" } */
  vfloat = 3.1; /* { dg-warning "conversion" } */
  ffloat (3.1L); /* { dg-warning "conversion" } */
  vfloat = 3.1L;  /* { dg-warning "conversion" } */
  fdouble (3.1L); /* { dg-warning "conversion" "" { target large_long_double } } */
  vdouble = 3.1L; /* { dg-warning "conversion" "" { target large_long_double } } */
  ffloat (vdouble); /* { dg-warning "conversion" } */
  vfloat = vdouble; /* { dg-warning "conversion" } */
  ffloat (vlongdouble); /* { dg-warning "conversion" } */
  vfloat = vlongdouble; /* { dg-warning "conversion" } */
  fdouble (vlongdouble); /* { dg-warning "conversion" "" { target large_long_double } } */
  vdouble = vlongdouble; /* { dg-warning "conversion" "" { target large_long_double } } */


  ffloat ((float) 3.1); 
  vfloat = (float) 3.1;
  ffloat ((float) 3.1L);
  vfloat = (float) 3.1L; 
  fdouble ((double) 3.1L); 
  vdouble = (double) 3.1L; 
  ffloat ((float) vdouble); 
  vfloat = (float) vdouble; 
  ffloat ((float) vlongdouble); 
  vfloat = (float) vlongdouble;
  fdouble ((double) vlongdouble);
  vdouble = (double) vlongdouble;


  ffloat (3.0);
  vfloat = 3.0;
  ffloat (3.1f);
  vfloat = 3.1f;
  ffloat (0.25L);
  vfloat = 0.25L;


  fdouble (3.0);
  vdouble = 3.0;
  fdouble (3.1f);
  vdouble = 3.1f;
  fdouble (0.25L);
  vdouble = 0.25L;

  flongdouble (3.0);
  vlongdouble = 3.0;
  flongdouble (3.1f);
  vlongdouble = 3.1f;
  flongdouble (0.25L);
  vlongdouble = 0.25L;

  ffloat (f);
  vfloat = f;
  fdouble (f);
  vdouble = f;
  fdouble (d);
  vdouble = d;
  flongdouble (f);
  vlongdouble = f;
  flongdouble (d);
  vlongdouble = d;
  flongdouble (ld);
  vlongdouble = ld;
}
