/* PR c/48956: Test for diagnostics for implicit conversions from complex
   to real types and narrowing conversions of complex types.  */

/* Architecture restrictions taken from Wconversion-real-integer.c.
   Likewise, the magic value 16777217.  */

/* { dg-do compile } */
/* { dg-skip-if "doubles are floats,ints are 16bits" { "avr-*-*" } } */
/* { dg-options " -std=c99 -pedantic -Wconversion -fno-trapping-math" } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target double64plus } */

/* A number which does not fit into float.  */
#define MAX_FLOAT_PLUS 16777217.

/* Other types could be added, but that won't affect test coverage.  */
void ffloatc (float _Complex);
void fdoublec (double _Complex);

void ffloat (float);
void fdouble (double);

void fsi (int);
void fui (unsigned);

float _Complex vfloatc;
double _Complex vdoublec;

float vfloat;
double vdouble;

int vsi;
unsigned vui;

/* Check implicit conversions of complex values to reals.  */
void
var_complex_to_real (void)
{
  float _Complex floatc = 0.;
  double _Complex doublec = 0.;

  ffloatc (floatc);
  fdoublec (doublec);
  vfloatc = floatc;
  vdoublec = doublec;

  ffloat (floatc); /* { dg-warning "conversion" } */
  fdouble (floatc); /* { dg-warning "conversion" } */
  vfloat = floatc; /* { dg-warning "conversion" } */
  vdouble = floatc; /* { dg-warning "conversion" } */

  ffloat (doublec); /* { dg-warning "conversion" } */
  fdouble (doublec); /* { dg-warning "conversion" } */
  vfloat = doublec; /* { dg-warning "conversion" } */
  vdouble = doublec; /* { dg-warning "conversion" } */
}

/* Check implicit narrowing conversions of complex values.  */
void
var_complex_narrowing (void)
{
  float _Complex floatc = 0.;
  double _Complex doublec = 0.;

  vdoublec = floatc;
  vfloatc = doublec; /* { dg-warning "conversion from .complex double. to .complex float. may change value" } */

  fdoublec (floatc);
  ffloatc (doublec); /* { dg-warning "conversion from .complex double. to .complex float. may change value" } */
}

/* Check implicit conversions of complex values to integers.  */
void
var_complex_to_int (void)
{
  float _Complex floatc = 0.;
  double _Complex doublec = 0.;

  fsi (floatc); /* { dg-warning "conversion" } */
  fui (floatc); /* { dg-warning "conversion" } */
  vsi = floatc; /* { dg-warning "conversion" } */
  vui = floatc; /* { dg-warning "conversion" } */

  fsi (doublec); /* { dg-warning "conversion" } */
  fui (doublec); /* { dg-warning "conversion" } */
  vsi = doublec; /* { dg-warning "conversion" } */
  vui = doublec; /* { dg-warning "conversion" } */
}

/* Check implicit conversion of constant complex values to floats.  */
void
const_complex_to_real (void)
{
  ffloat (__builtin_complex (0., 1.)); /* { dg-warning "conversion" } */
  fdouble (__builtin_complex (0., 1.)); /* { dg-warning "conversion" } */

  vfloat = __builtin_complex (0., 1.); /* { dg-warning "conversion" } */
  vdouble = __builtin_complex (0., 1.); /* { dg-warning "conversion" } */

  vfloat = __builtin_complex (1., 0.) + __builtin_complex (1., 0.);
  vdouble = __builtin_complex (0., 0.) * __builtin_complex (1., 1.);
  ffloat (__builtin_complex (1., 0.) + __builtin_complex (1., 0.));
  fdouble (__builtin_complex (1., 0.) + __builtin_complex (1., 0.));

  vfloat = __builtin_complex (MAX_FLOAT_PLUS, 0.); /* { dg-warning "float-conversion" } */
  ffloat (__builtin_complex (MAX_FLOAT_PLUS, 0.)); /* { dg-warning "float-conversion" } */
}

/* Check implicit conversion of constant complex values to integers.  */
void
const_complex_to_int (void)
{
  vsi = __builtin_complex (-1., 0.);
  vui = __builtin_complex (1., 0.);
  fsi (__builtin_complex (-1., 0.));
  fui (__builtin_complex (1., 0.));

  vui = __builtin_complex (-1., 0.); /* { dg-warning "overflow" } */
  fui (__builtin_complex (-1., 0.)); /* { dg-warning "overflow" } */

  vsi = __builtin_complex (0.5, 0.); /* { dg-warning "float-conversion" } */
  fui (__builtin_complex (0.5, 0.)); /* { dg-warning "float-conversion" } */

  vsi = __builtin_complex (-0.5, 0.); /* { dg-warning "float-conversion" } */
  fui (__builtin_complex (-0.5, 0.)); /* { dg-warning "float-conversion" } */
}

/* Check implicit narrowing conversion of constant complex values to.  */
void
const_complex_narrowing (void)
{
  ffloatc (__builtin_complex (-100., 100.));

  ffloatc (__builtin_complex (MAX_FLOAT_PLUS, 0.)); /* { dg-warning "float-conversion" } */
  ffloatc (__builtin_complex (0., MAX_FLOAT_PLUS)); /* { dg-warning "float-conversion" } */
  ffloatc (__builtin_complex (MAX_FLOAT_PLUS, MAX_FLOAT_PLUS)); /* { dg-warning "float-conversion" } */
}

