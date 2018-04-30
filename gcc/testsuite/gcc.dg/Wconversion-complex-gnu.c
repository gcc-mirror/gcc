/* PR c/48956: Test for diagnostics for implicit conversions involving complex
   types.  See also Wconversion-complex-c99.c.

   These tests cover integer complex values (which are GNU extensions).  */

/* { dg-do compile } */
/* { dg-skip-if "doubles are floats,ints are 16bits" { "avr-*-*" } } */
/* { dg-options " -std=gnu99 -Wconversion " } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target double64plus } */

#include <limits.h>

void fsi (int);
void fui (unsigned);
void ffloat (float);
int vsi;
unsigned int vui;
float vfloat;

void fsic (int _Complex);
void fuic (unsigned _Complex);
void ffloatc (float _Complex);
int _Complex vsic;
unsigned _Complex vuic;
float _Complex vfloatc;

/* Check implicit conversions of float complex-domain values to integer
   complex-domain types.  */
void
var_float_to_int (void)
{
  double _Complex doublec = 0.;

  fsic (doublec); /* { dg-warning "conversion" } */
  fuic (doublec); /* { dg-warning "conversion" } */

  vsic = doublec; /* { dg-warning "conversion" } */
  vuic = doublec; /* { dg-warning "conversion" } */
}

/* Check implicit conversions of integer complex-domain values to integer
   real-domain types.  */
void
var_complex_to_real (void)
{
  int _Complex ic = 0;
  unsigned _Complex uc = 0;
  unsigned long long _Complex ullc = 0;

  fsic (ic);
  fuic (uc);
  vsic = ic;
  vuic = uc;

  fsi (ic); /* { dg-warning "conversion" } */
  vsi = ic; /* { dg-warning "conversion" } */
  fui (uc); /* { dg-warning "conversion" } */
  vui = uc; /* { dg-warning "conversion" } */

  fuic (ullc); /* { dg-warning "conversion" } */
  vuic = ullc; /* { dg-warning "conversion" } */

  fui (ic); /* { dg-warning "conversion" } */
  vui = ic; /* { dg-warning "conversion" } */
}

/* Check implicit conversions of float complex-domain constants to integer
   types.  */
void
const_float_to_int (void)
{
  fsic (1. - 1.i);
  fuic (1. + 1.i);
  vsic = 1. - 1.i;
  vuic = 1. + 1.i;

  fsic (0.5 + 0.i); /* { dg-warning "conversion" } */
  vsic = 0.5 + 0.i; /* { dg-warning "conversion" } */
  fuic (0.5 + 0.i); /* { dg-warning "conversion" } */
}

/* Check implicit conversions of integer complex-domain constants to integer
   types.  */
void
const_complex_int_to_real_int (void)
{
  fsi (-1 + 0i);
  fui (1 + 0i);
  vsi = -1 + 0i;
  vui = 1 + 0i;

  fui (1 + 1i); /* { dg-warning "conversion" } */
  vui = 1 + 1i; /* { dg-warning "conversion" } */

  fui (UINT_MAX + 1ull + 0i); /* { dg-warning "conversion" } */
  vui = UINT_MAX + 1ull + 0i; /* { dg-warning "conversion" } */

  ffloat (UINT_MAX + 0i); /* { dg-warning "conversion" } */
  vfloat = UINT_MAX + 0i; /* { dg-warning "conversion" } */
}

void
const_complex_int_narrowing (void)
{
  fsic (1 - 1i);
  fuic (1 + 1i);
  vsic = 1 - 1i;
  vuic = 1 + 1i;

  fuic (UINT_MAX + 1ull + 1i); /* { dg-warning "conversion" } */
  fuic ((UINT_MAX + 1ull) * 1i); /* { dg-warning "conversion" } */
  fuic ((UINT_MAX + 1ull) + (UINT_MAX + 1ull) * 1i); /* { dg-warning "conversion" } */

  vuic = (UINT_MAX + 1ull) * 1i; /* { dg-warning "conversion" } */
  vuic = (UINT_MAX + 1ull) + 1i; /* { dg-warning "conversion" } */
  vuic = (UINT_MAX + 1ull) + (UINT_MAX + 1ull) * 1i; /* { dg-warning "conversion" } */

  ffloatc (UINT_MAX * 1i); /* { dg-warning "conversion" } */
  ffloatc (UINT_MAX + 1i); /* { dg-warning "conversion" } */
  ffloatc (UINT_MAX + UINT_MAX * 1i); /* { dg-warning "conversion" } */

  vfloatc = UINT_MAX * 1i; /* { dg-warning "conversion" } */
  vfloatc = UINT_MAX + 1i; /* { dg-warning "conversion" } */
  vfloatc = UINT_MAX + UINT_MAX * 1i; /* { dg-warning "conversion" } */
}

