/* Test for _Complex: in C99 only.  Test for increment and decrement.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* Use of ++ and -- on complex types (both prefix and postfix) is a
   C99 constraint violation (6.5.2.4p1, 6.5.3.1p1).
*/

_Complex double
foo (_Complex double z)
{
  z++; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "complex" "postinc" { target *-*-* } .-1 } */
  ++z; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "complex" "preinc" { target *-*-* } .-1 } */
  z--; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "complex" "postdec" { target *-*-* } .-1 } */
  --z; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "complex" "predec" { target *-*-* } .-1 } */
  return z;
}
