/*  Check -ff2c calling conventions
    Return value of COMPLEX function is via an extra argument in the
     calling sequence that points to where to store the return value
    Additional underscore appended to function name
  
   Simplified from f2c output and tested with g77 */

/* We used to #include <complex.h>, but this fails for some platforms
   (like cygwin) who don't have it yet.  */
#define complex __complex__
#define _Complex_I (1.0iF)

typedef float real;
typedef double doublereal;

extern double f2c_4b__(double *);
extern void f2c_4d__( complex float *, complex float *);
extern void f2c_4f__( complex float *, int *,complex float *);
extern void f2c_4h__( complex double *, complex double *);
extern void f2c_4j__( complex double *, int *, complex double *);
extern void abort (void);

void f2c_4a__(void) {
  double a,b;
  a = 1023.0;
  b=f2c_4b__(&a);
  if ( a != b ) abort();
}

void f2c_4c__(void) {
  complex float x,ret_val;
  x = 1234 + 5678 * _Complex_I;
  f2c_4d__(&ret_val,&x);
  if ( x != ret_val ) abort();
}

void f2c_4e__(void) {
  complex float x,ret_val;
  int i=0;
  x = 1234 + 5678 * _Complex_I;
  f2c_4f__(&ret_val,&i,&x);
  if ( x != ret_val ) abort();
}

void f2c_4g__(void) {
  complex double x,ret_val;
  x = 1234 + 5678.0f * _Complex_I;
  f2c_4h__(&ret_val,&x);
  if ( x != ret_val ) abort();
}

void f2c_4i__(void) {
  complex double x,ret_val;
  int i=0;
  x = 1234.0f + 5678.0f * _Complex_I;
  f2c_4j__(&ret_val,&i,&x);
  if ( x != ret_val ) abort();
}

void f2c_4k__(complex float *ret_val, complex float *x) {
  *ret_val = *x;
}

void f2c_4l__(complex float *ret_val, int *i, complex float *x) {
  *ret_val = *x;
}

void f2c_4m__(complex double *ret_val, complex double *x) {
  *ret_val = *x;
}

void f2c_4n__(complex double *ret_val, int *i, complex double *x) {
  *ret_val = *x;
}
