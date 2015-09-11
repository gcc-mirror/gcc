/* Test for constant expressions: VLA size constraints with
   -frounding-math.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -frounding-math" } */
/* { dg-require-effective-target alloca } */

void
f (void)
{
  /* With -frounding-math, presume that floating-point expressions
     that may depend on the rounding mode do not count as arithmetic
     constant expressions, and so arrays involving such expressions in
     their sizes do not have the size checked for being negative.  */
  int a1[(int)(-5.0/3.0)];
}
