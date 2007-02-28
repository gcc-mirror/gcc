/*  Passing from fortran to C by value, using %VAL.  */

typedef struct { float r, i; } complex;
typedef struct { double r, i; } complex8;
extern void f_to_f__ (float*, float, float*, float**);
extern void f_to_f8__ (double*, double, double*, double**);
extern void i_to_i__ (int*, int, int*, int**);
extern void i_to_i8__ (long*, long, long*, long**);
extern void c_to_c__ (complex*, complex, complex*, complex**);
extern void c_to_c8__ (complex8*, complex8, complex8*, complex8**);
extern void abort (void);

void
f_to_f__(float *retval, float a1, float *a2, float **a3)
{
  if ( a1 != *a2 ) abort();
  if ( a1 != **a3 ) abort();
  a1 = 0.0;
  *retval = *a2 * 2.0;
  return;
}

void
f_to_f8__(double *retval, double a1, double *a2, double **a3)
{
  if ( a1 != *a2 ) abort();
  if ( a1 != **a3 ) abort();
  a1 = 0.0;
  *retval = *a2 * 2.0;
  return;
}

void
i_to_i__(int *retval, int i1, int *i2, int **i3)
{
  if ( i1 != *i2 ) abort();
  if ( i1 != **i3 ) abort();
  i1 = 0;
  *retval = *i2 * 3;
  return;
}

void
i_to_i8__(long *retval, long i1, long *i2, long **i3)
{
  if ( i1 != *i2 ) abort();
  if ( i1 != **i3 ) abort();
  i1 = 0;
  *retval = *i2 * 3;
  return;
}

void
c_to_c__(complex *retval, complex c1, complex *c2, complex **c3)
{
  if ( c1.r != c2->r ) abort();
  if ( c1.i != c2->i ) abort();
  if ( c1.r != (*c3)->r ) abort();
  if ( c1.i != (*c3)->i ) abort();
  c1.r = 0.0;
  c1.i = 0.0;
  retval->r = c2->r * 4.0;
  retval->i = c2->i * 4.0;
  return;
}

void
c_to_c8__(complex8 *retval, complex8 c1, complex8 *c2, complex8 **c3)
{
  if ( c1.r != c2->r ) abort();
  if ( c1.i != c2->i ) abort();
  if ( c1.r != (*c3)->r ) abort();
  if ( c1.i != (*c3)->i ) abort();
  c1.r = 0.0;
  c1.i = 0.0;
  retval->r = c2->r * 4.0;
  retval->i = c2->i * 4.0;
  return;
}
