/* Test for diagnostics for Wconversion between floating-point and
   integers.  */

/* { dg-do compile }
/* { dg-options "-std=c99 -Wconversion" } */

#include <limits.h>

void fsi (signed int x);
void fui (unsigned int x);
void ffloat (float x);
void fdouble (double x);

float  vfloat;
double vdouble;

void h (void)
{
  unsigned int ui = 3;
  int   si = 3;
  unsigned char uc = 3;
  signed char sc = 3;
  float  f = 3;
  double d = 3;

  fsi (3.1f); /* { dg-warning "conversion" } */
  si = 3.1f; /* { dg-warning "conversion" } */
  fsi (3.1);  /* { dg-warning "conversion" } */
  si = 3.1;  /* { dg-warning "conversion" } */
  fsi (d);    /* { dg-warning "conversion" } */
  si = d;    /* { dg-warning "conversion" } */
  fui (-1.0); /* { dg-warning "overflow" } */
  ui = -1.0;   /* { dg-warning "overflow" } */
  ffloat (INT_MAX);  /* { dg-warning "conversion" } */
  vfloat = INT_MAX;  /* { dg-warning "conversion" } */
  ffloat (16777217); /* { dg-warning "conversion" } */
  vfloat = 16777217; /* { dg-warning "conversion" } */
  ffloat (si); /* { dg-warning "conversion" } */
  vfloat = si; /* { dg-warning "conversion" } */
  ffloat (ui); /* { dg-warning "conversion" } */
  vfloat = ui; /* { dg-warning "conversion" } */

  fsi (3);
  si = 3;
  fsi (3.0f);
  si = 3.0f;
  fsi (3.0);
  si = 3.0;
  fsi (16777217.0f);
  si = 16777217.0f;
  fsi ((int) 3.1);
  si = (int) 3.1;
  ffloat (3U);
  vfloat = 3U;
  ffloat (3);
  vfloat = 3;
  ffloat (INT_MIN);
  vfloat = INT_MIN;
  ffloat (uc);
  vfloat = uc;
  ffloat (sc);
  vfloat = sc;

  fdouble (UINT_MAX);
  vdouble = UINT_MAX;
  fdouble (ui);
  vdouble = ui;
  fdouble (si);
  vdouble = si;
}


