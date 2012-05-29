/* { dg-do compile } */
/* { dg-options "-Wdouble-promotion -ftrack-macro-expansion=2" } */

#include <stddef.h>

/* Some targets do not provide <complex.h> so we define I ourselves.  */
#define I 1.0iF
#define ID ((_Complex double)I) // { dg-warning "implicit" }

float f;
double d;
int i;
long double ld;
_Complex float cf;
_Complex double cd;
_Complex long double cld;
size_t s;

extern void varargs_fn (int, ...);
extern void double_fn (double);
extern float float_fn (void);

void 
usual_arithmetic_conversions(void) 
{
  float local_f;
  _Complex float local_cf;

  /* Values of type "float" are implicitly converted to "double" or
     "long double" due to use in arithmetic with "double" or "long
     double" operands.  */
  local_f = f + 1.0;         /* { dg-warning "implicit" } */
  local_f = f - d;           /* { dg-warning "implicit" } */
  local_f = 1.0f * 1.0;      /* { dg-warning "implicit" } */
  local_f = 1.0f / d;        /* { dg-warning "implicit" } */

  local_cf = cf + 1.0;       /* { dg-warning "implicit" } */
  local_cf = cf - d;         /* { dg-warning "implicit" } */
  local_cf = cf + 1.0 * ID;  /* { dg-message "in expansion of macro 'ID'" } */
  local_cf = cf - cd;        /* { dg-warning "implicit" } */
  
  local_f = i ? f : d;       /* { dg-warning "implicit" } */
  i = f == d;                /* { dg-warning "implicit" } */
  i = d != f;                /* { dg-warning "implicit" } */
}

void 
default_argument_promotion (void) 
{
  /* Because "f" is part of the variable argument list, it is promoted
     to "double".  */
  varargs_fn (1, f);   /* { dg-warning "implicit" } */
}

/* There is no warning when an explicit cast is used to perform the
   conversion.  */

void
casts (void) 
{
  float local_f;
  _Complex float local_cf;

  local_f = (double)f + 1.0;                 /* { dg-bogus "implicit" } */
  local_f = (double)f - d;                   /* { dg-bogus "implicit" } */
  local_f = (double)1.0f + 1.0;              /* { dg-bogus "implicit" } */
  local_f = (double)1.0f - d;                /* { dg-bogus "implicit" } */

  local_cf = (_Complex double)cf + 1.0;      /* { dg-bogus "implicit" } */
  local_cf = (_Complex double)cf - d;        /* { dg-bogus "implicit" } */
  local_cf = (_Complex double)cf + 1.0 * ID; /* { dg-bogus "implicit" } */
  local_cf = (_Complex double)cf - cd;       /* { dg-bogus "implicit" } */

  local_f = i ? (double)f : d;               /* { dg-bogus "implicit" } */
  i = (double)f == d;                        /* { dg-bogus "implicit" } */
  i = d != (double)f;                        /* { dg-bogus "implicit" } */
}

/* There is no warning on conversions that occur in assignment (and
   assignment-like) contexts.  */

void 
assignments (void)
{
  d = f;           /* { dg-bogus "implicit" } */
  double_fn (f);   /* { dg-bogus "implicit" } */
  d = float_fn (); /* { dg-bogus "implicit" } */
}

/* There is no warning in non-evaluated contexts.  */

void
non_evaluated (void)
{
  s = sizeof (f + 1.0);             /* { dg-bogus "implicit" } */
  s = __alignof__ (f + 1.0);        /* { dg-bogus "implicit" } */
  d = (__typeof__(f + 1.0))f;       /* { dg-bogus "implicit" } */
  s = sizeof (i ? f : d);           /* { dg-bogus "implicit" } */
}
