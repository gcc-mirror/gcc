/* Test C2X support for empty initializers: invalid use cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* Empty initialization is invalid for arrays of unknown size.  This is
   diagnosed via the diagnostic for zero-size arrays.  */
int x[] = {}; /* { dg-error "zero or negative size array" } */

void
f (int a)
{
  int x1[] = {}; /* { dg-error "zero or negative size array" } */
  int x2[][a] = {}; /* { dg-error "zero or negative size array" } */
  /* Nonempty VLA initializers are still invalid.  */
  int x3[a] = { 0 }; /* { dg-error "variable-sized object may not be initialized except with an empty initializer" } */
  /* Variable-size compound literals are still invalid.  */
  (void) (int [a]) {}; /* { dg-error "compound literal has variable size" } */
}
