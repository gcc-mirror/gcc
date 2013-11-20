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

  ffloat (3.1); /* { dg-warning "conversion to 'float' alters 'double' constant value" } */
  vfloat = 3.1; /* { dg-warning "conversion to 'float' alters 'double' constant value" } */
  ffloat (3.1L); /* { dg-warning "conversion to 'float' alters 'long double' constant value" } */
  vfloat = 3.1L;  /* { dg-warning "conversion to 'float' alters 'long double' constant value" } */
  fdouble (3.1L); /* { dg-warning "conversion to 'double' alters 'long double' constant value" "" { target large_long_double } } */
  vdouble = 3.1L; /* { dg-warning "conversion to 'double' alters 'long double' constant value" "" { target large_long_double } } */
  ffloat (vdouble); /* { dg-warning "conversion to 'float' from 'double' may alter its value" } */
  vfloat = vdouble; /* { dg-warning "conversion to 'float' from 'double' may alter its value" } */
  ffloat (vlongdouble); /* { dg-warning "conversion to 'float' from 'long double' may alter its value" } */
  vfloat = vlongdouble; /* { dg-warning "conversion to 'float' from 'long double' may alter its value" } */
  fdouble (vlongdouble); /* { dg-warning "conversion to 'double' from 'long double' may alter its value" "" { target large_long_double } } */
  vdouble = vlongdouble; /* { dg-warning "conversion to 'double' from 'long double' may alter its value" "" { target large_long_double } } */

  fsi (3.1f); /* { dg-warning "conversion to 'int' alters 'float' constant value" } */
  si = 3.1f; /* { dg-warning "conversion to 'int' alters 'float' constant value" } */
  fsi (3.1);  /* { dg-warning "conversion to 'int' alters 'double' constant value" } */
  si = 3.1;  /* { dg-warning "conversion to 'int' alters 'double' constant value" } */
  fsi (d);    /* { dg-warning "conversion to 'int' from 'double' may alter its value" } */
  si = d;    /* { dg-warning "conversion to 'int' from 'double' may alter its value" } */
  ffloat (INT_MAX);  /* { dg-warning "conversion to 'float' alters 'int' constant value" } */
  vfloat = INT_MAX;  /* { dg-warning "conversion to 'float' alters 'int' constant value" } */
  ffloat (16777217); /* { dg-warning "conversion to 'float' alters 'int' constant value" } */
  vfloat = 16777217; /* { dg-warning "conversion to 'float' alters 'int' constant value" } */

  sc = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion to 'signed char' alters 'double' constant value" } */
  uc = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion to 'unsigned char' alters 'double' constant value" } */
}
