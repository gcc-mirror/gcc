/*  Passing from fortran to C by value, using VALUE.  This is identical
    to c_by_val_1.c, which performs the same function for %VAL.

    Contributed by Paul Thomas <pault@gcc.gnu.org>  */

/* We used to #include <complex.h>, but this fails for some platforms
   (like cygwin) who don't have it yet.  */
#define complex __complex__
#define _Complex_I (1.0iF)

extern float *f_to_f__ (float, float*);
extern int *i_to_i__ (int, int*);
extern void c_to_c__ (complex float*, complex float, complex float*);
extern void abort (void);

/* In f_to_f and i_to_i we return the second argument, so that we do
   not have to worry about keeping track of memory allocation between
   fortran and C.  All three functions check that the argument passed
   by value is the same as that passed by reference.  Then the passed
   by value argument is modified so that the caller can check that
   its version has not changed.*/

float *
f_to_f__(float a1, float *a2)
{
  if ( a1 != *a2 ) abort();
  *a2 = a1 * 2.0;
  a1 = 0.0;
  return a2;
}

int *
i_to_i__(int i1, int *i2)
{
  if ( i1 != *i2 ) abort();
  *i2 = i1 * 3;
  i1 = 0;
  return i2;
}

void
c_to_c__(complex float *retval, complex float c1, complex float *c2)
{
  if ( c1 != *c2 ) abort();
  c1 = 0.0 + 0.0 * _Complex_I;
  *retval = *c2 * 4.0;
  return;
}

