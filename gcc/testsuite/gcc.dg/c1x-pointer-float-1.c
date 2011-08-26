/* Test C1X constraint against pointer / floating-point casts.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

void *p;
float f;
double d;
long double ld;
_Complex float cf;
_Complex double cd;
_Complex long double cld;

void
func (void)
{
  f = (float) p; /* { dg-error "pointer" } */
  d = (double) p; /* { dg-error "pointer" } */
  ld = (long double) p; /* { dg-error "pointer" } */
  cf = (_Complex float) p; /* { dg-error "pointer" } */
  cd = (_Complex double) p; /* { dg-error "pointer" } */
  cld = (_Complex long double) p; /* { dg-error "pointer" } */
  p = (void *) f; /* { dg-error "pointer" } */
  p = (void *) d; /* { dg-error "pointer" } */
  p = (void *) ld; /* { dg-error "pointer" } */
  p = (void *) cf; /* { dg-error "pointer" } */
  p = (void *) cd; /* { dg-error "pointer" } */
  p = (void *) cld; /* { dg-error "pointer" } */
}
