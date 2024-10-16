/* Test for prototype followed by old-style definition, as in
   dremf-type-compat-1.c but with a non-built-in function.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

float f (float, float);

float
f (x, y)
     float x, y;
{
  return x + y;
}
