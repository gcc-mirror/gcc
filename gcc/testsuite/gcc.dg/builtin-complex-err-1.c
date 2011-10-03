/* Test __builtin_complex errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

typedef double D;

double d;

_Complex double dc = __builtin_complex (1.0, (D) 0.0);

_Complex double dc2 = __builtin_complex (d, 0.0); /* { dg-error "not constant" } */

_Complex float fc = __builtin_complex (1.0f, 1); /* { dg-error "not of real binary floating-point type" } */

_Complex float fc2 = __builtin_complex (1, 1.0f); /* { dg-error "not of real binary floating-point type" } */

_Complex float fc3 = __builtin_complex (1.0f, 1.0); /* { dg-error "different types" } */

void
f (void)
{
  __builtin_complex (0.0); /* { dg-error "wrong number of arguments" } */
  __builtin_complex (0.0, 0.0, 0.0); /* { dg-error "wrong number of arguments" } */
}

void (*p) (void) = __builtin_complex; /* { dg-error "cannot take address" } */
