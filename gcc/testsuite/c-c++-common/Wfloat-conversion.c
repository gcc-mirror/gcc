/* Test for diagnostics for Wconversion for floating-point.  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -Wfloat-conversion" { target c } } */
/* { dg-options "-Wfloat-conversion" { target c++ } } */
/* { dg-require-effective-target large_double } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target double64plus } */
#include <limits.h>

float  vfloat;
double vdouble;
long double vlongdouble;
int bar;

void fsi (signed int x);
void fui (unsigned int x);
void ffloat (float f);
void fdouble (double d);
void flongdouble (long double ld);

void h (void)
{
  unsigned int ui = 3;
  int   si = 3;
  unsigned char uc = 3;
  signed char sc = 3;
  float f = 0;
  double d = 0;
  long double ld = 0;

  ffloat (3.1); /* { dg-warning "conversion from .double. to .float. changes value" } */
  vfloat = 3.1; /* { dg-warning "conversion from .double. to .float. changes value" } */
  ffloat (3.1L); /* { dg-warning "conversion from .long double. to .float. changes value" } */
  vfloat = 3.1L;  /* { dg-warning "conversion from .long double. to .float. changes value" } */
  fdouble (3.1L); /* { dg-warning "conversion from .long double. to .double. changes value" "" { target large_long_double } } */
  vdouble = 3.1L; /* { dg-warning "conversion from .long double. to .double. changes value" "" { target large_long_double } } */
  ffloat (vdouble); /* { dg-warning "conversion from .double. to .float. may change value" } */
  vfloat = vdouble; /* { dg-warning "conversion from .double. to .float. may change value" } */
  ffloat (vlongdouble); /* { dg-warning "conversion from .long double. to .float. may change value" } */
  vfloat = vlongdouble; /* { dg-warning "conversion from .long double. to .float. may change value" } */
  fdouble (vlongdouble); /* { dg-warning "conversion from .long double. to .double. may change value" "" { target large_long_double } } */
  vdouble = vlongdouble; /* { dg-warning "conversion from .long double. to .double. may change value" "" { target large_long_double } } */

  fsi (3.1f); /* { dg-warning "conversion from .float. to .int. changes value" } */
  si = 3.1f; /* { dg-warning "conversion from .float. to .int. changes value" } */
  fsi (3.1);  /* { dg-warning "conversion from .double. to .int. changes value" } */
  si = 3.1;  /* { dg-warning "conversion from .double. to .int. changes value" } */
  fsi (d);    /* { dg-warning "conversion from .double. to .int. may change value" } */
  si = d;    /* { dg-warning "conversion from .double. to .int. may change value" } */
  ffloat (INT_MAX);  /* { dg-warning "conversion from .int. to .float. changes value" } */
  vfloat = INT_MAX;  /* { dg-warning "conversion from .int. to .float. changes value" } */
  ffloat (16777217); /* { dg-warning "conversion from .int. to .float. changes value from .16777217." } */
  vfloat = 16777217; /* { dg-warning "conversion from .int. to .float. changes value from .16777217." } */

  sc = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion from .double. to .signed char. changes the value of .2\.1" } */
  uc = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion from .double. to .unsigned char. changes the value of .2\.1" } */
}
