/*  Check -ff2c calling conventions
    Return value of COMPLEX function is via an extra argument in the
     calling sequence that points to where to store the return value
    Additional underscore appended to function name
  
   Simplified from f2c output and tested with g77 */

typedef float real;
typedef double doublereal;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;

extern double f2c_4b__(double *);
extern void f2c_4d__( complex *, complex *);
extern void f2c_4f__( complex *, int *,complex *);
extern void f2c_4h__( doublecomplex *, doublecomplex *);
extern void f2c_4j__( doublecomplex *, int *, doublecomplex *);
extern void abort (void);

void f2c_4a__(void) {
  double a,b;
  a = 1023.0;
  b=f2c_4b__(&a);
  if ( a != b ) abort();
}

void f2c_4c__(void) {
  complex x,ret_val;
  x.r = 1234;
  x.i = 5678;
  f2c_4d__(&ret_val,&x);
  if ( x.r != ret_val.r && x.i != ret_val.i ) abort();
}

void f2c_4e__(void) {
  complex x,ret_val;
  int i=0;
  x.r = 1234;
  x.i = 5678;
  f2c_4f__(&ret_val,&i,&x);
  if ( x.r != ret_val.r && x.i != ret_val.i ) abort();
}

void f2c_4g__(void) {
  doublecomplex x,ret_val;
  x.r = 1234;
  x.i = 5678.0f;
  f2c_4h__(&ret_val,&x);
  if ( x.r != ret_val.r && x.i != ret_val.i ) abort();
}

void f2c_4i__(void) {
  doublecomplex x,ret_val;
  int i=0;
  x.r = 1234.0f;
  x.i = 5678.0f;
  f2c_4j__(&ret_val,&i,&x);
  if ( x.r != ret_val.r && x.i != ret_val.i ) abort();
}

void f2c_4k__(complex *ret_val, complex *x) {
  ret_val->r = x->r;
  ret_val->i = x->i;
}

void f2c_4l__(complex *ret_val, int *i, complex *x) {
  ret_val->r = x->r;
  ret_val->i = x->i;
}

void f2c_4m__(doublecomplex *ret_val, doublecomplex *x) {
  ret_val->r = x->r;
  ret_val->i = x->i;
}

void f2c_4n__(doublecomplex *ret_val, int *i, doublecomplex *x) {
  ret_val->r = x->r;
  ret_val->i = x->i;
}
