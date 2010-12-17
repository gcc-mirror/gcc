/* PR 23572 : warnings for out of range floating-point constants.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */
/* { dg-skip-if "No Inf support" { spu-*-* } } */
#include <math.h>

#ifndef INFINITY
#define INFINITY (__builtin_inff ())
#endif

void overflow(void)
{
  float f1 = 3.5E+38f;  /* { dg-warning "floating constant exceeds range" } */
  float f2 = -3.5E+38f; /* { dg-warning "floating constant exceeds range" } */
  float f3 = INFINITY;
  float f4 = -INFINITY;

  double d1 = 1.9E+308;  /* { dg-warning "floating constant exceeds range" } */
  double d2 = -1.9E+308; /* { dg-warning "floating constant exceeds range" } */
  double d3 = INFINITY;
  double d4 = -INFINITY;
}

void underflow(void)
{
  float f11 = 3.3E-10000000000000000000f;  /* { dg-warning "floating constant truncated to zero" } */
  float f22 = -3.3E-10000000000000000000f; /* { dg-warning "floating constant truncated to zero" } */
  float f1 = 3.3E-46f;  /* { dg-warning "floating constant truncated to zero" } */
  float f2 = -3.3E-46f; /* { dg-warning "floating constant truncated to zero" } */
  float f3 = 0;
  float f4 = -0;
  float f5 = 0.0;
  float f6 = -0.0;

  double d11 = 3.3E-10000000000000000000;  /* { dg-warning "floating constant truncated to zero" } */
  double d22 = -3.3E-10000000000000000000; /* { dg-warning "floating constant truncated to zero" } */
  double d1 = 1.4E-325;  /* { dg-warning "floating constant truncated to zero" } */
  double d2 = -1.4E-325; /* { dg-warning "floating constant truncated to zero" } */
  double d3 = 0;
  double d4 = -0;
  double d5 = 0.0;
  double d6 = -0.0;
}
