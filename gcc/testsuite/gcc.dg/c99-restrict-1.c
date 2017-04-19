/* Test for restrict: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

char *restrict foo;

/* The following are constraint violations and should be rejected.  */

int restrict bar; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "restrict" "restrict constraint violation" { target *-*-* } .-1 } */

typedef void (*fp) (void);

fp restrict baz; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "restrict" "restrict constraint violation" { target *-*-* } .-1 } */

void quux (int restrict a[3]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "restrict" "restrict constraint violation" { target *-*-* } .-1 } */
