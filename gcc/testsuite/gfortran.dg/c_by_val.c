/*  Passing from fortran to C by value, using %VAL.  */

#include <inttypes.h>
#include <complex.h>

extern void f_to_f__ (float*, float, float*, float**);
extern void f_to_f8__ (double*, double, double*, double**);
extern void i_to_i__ (int*, int, int*, int**);
extern void i_to_i8__ (int64_t*, int64_t, int64_t*, int64_t**);
extern void c_to_c__ (float _Complex*, float _Complex, float _Complex*, float _Complex**);
extern void c_to_c8__ (double _Complex*, double _Complex, double _Complex*, double _Complex**);
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
i_to_i8__(int64_t *retval, int64_t i1, int64_t *i2, int64_t **i3)
{
  if ( i1 != *i2 ) abort();
  if ( i1 != **i3 ) abort();
  i1 = 0;
  *retval = *i2 * 3;
  return;
}

void
c_to_c__(float _Complex *retval, float _Complex c1, float _Complex *c2, float _Complex **c3)
{
  if ( c1 != *c2    ) abort();
  if ( c1 != *(*c3) ) abort();
  c1 = 0.0 + 0.0 * _Complex_I;
  *retval = (*c2) * 4.0;
  return;
}

void
c_to_c8__(double _Complex *retval, double _Complex c1, double _Complex *c2, double _Complex **c3)
{
  if ( c1 != *c2    ) abort();
  if ( c1 != *(*c3) ) abort();
  c1 = 0.0 +  0.0 * _Complex_I;;
  *retval = (*c2) * 4.0;
  return;
}
