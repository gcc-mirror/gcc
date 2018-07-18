/* Test for _Complex: in C99 only.  A few basic tests.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* Test _Complex allowed on floating types.  */

float _Complex a;
_Complex float b;
double _Complex c;
_Complex double d;
long double _Complex e;
_Complex long double f;

/* Plain `_Complex' for complex double is a GNU extension.  */
_Complex g; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "plain" "plain _Complex" { target *-*-* } .-1 } */

/* Complex integer types are GNU extensions.  */
_Complex int h; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "complex integer" "_Complex int" { target *-*-* } .-1 } */
_Complex long i; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "complex integer" "_Complex long" { target *-*-* } .-1 } */

/* Use of ~ for complex conjugation is a GNU extension, but a constraint
   violation (6.5.3.3p1) in C99.
*/
_Complex double
foo (_Complex double z)
{
  return ~z; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "complex conj" "~ for conjugation" { target *-*-* } .-1 } */
}
