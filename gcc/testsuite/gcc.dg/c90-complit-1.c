/* Test for compound literals: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { int a; int b; };
union u { int c; int d; };

void
foo (void)
{
  (int) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "compound literal" "scalar" { target *-*-* } 12 } */
  (struct s) { 1, 2 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "compound literal" "struct" { target *-*-* } 14 } */
  (union u) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "compound literal" "union" { target *-*-* } 16 } */
  (int [1]) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "compound literal" "array" { target *-*-* } 18 } */
}
