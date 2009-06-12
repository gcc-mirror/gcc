/* Test for prototype followed by old-style definition, as in
   dremf-type-compat-3.c but with a non-built-in function.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

float f (float, float); /* { dg-error "prototype declaration" } */

float
f (x, y)
     float x; /* { dg-error "promoted argument 'x' doesn't match prototype" } */
     float y; /* { dg-error "promoted argument 'y' doesn't match prototype" } */
{
  return x + y;
}
