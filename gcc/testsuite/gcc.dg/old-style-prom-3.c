/* Test for prototype followed by old-style definition, as in
   dremf-type-compat-3.c but with a non-built-in function.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

float f (float, float); /* { dg-error "prototype declaration" } */

float
f (x, y)
     float x;
     float y;
{ /* { dg-error "promoted argument '.' doesn't match prototype" } */
  return x + y;
}
