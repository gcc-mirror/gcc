/* Test for non-lvalue arrays decaying to pointers: in C99 only.
   Test various ways of producing non-lvalue arrays.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { char c[1]; };
struct s a, b, c;
int d;

void
bar (void)
{
  char *t;
  (d ? b : c).c[0]; /* { dg-bogus "warning" "warning in place of error" } */
  (d, b).c[0]; /* { dg-bogus "warning" "warning in place of error" } */
  (a = b).c[0]; /* { dg-bogus "warning" "warning in place of error" } */
  t = (d ? b : c).c; /* { dg-bogus "warning" "warning in place of error" } */
  t = (d, b).c; /* { dg-bogus "warning" "warning in place of error" } */
  t = (a = b).c; /* { dg-bogus "warning" "warning in place of error" } */
  (d ? b : c).c + 1; /* { dg-bogus "warning" "warning in place of error" } */
  (d, b).c + 1; /* { dg-bogus "warning" "warning in place of error" } */
  (a = b).c + 1; /* { dg-bogus "warning" "warning in place of error" } */
}
/* { dg-error "non-lvalue" "array not decaying to lvalue" { target *-*-* } 15 }
   { dg-error "non-lvalue" "array not decaying to lvalue" { target *-*-* } 16 }
   { dg-error "non-lvalue" "array not decaying to lvalue" { target *-*-* } 17 }
   { dg-error "non-lvalue|incompatible" "array not decaying to lvalue" { target *-*-* } 18 }
   { dg-error "non-lvalue|incompatible" "array not decaying to lvalue" { target *-*-* } 19 }
   { dg-error "non-lvalue|incompatible" "array not decaying to lvalue" { target *-*-* } 20 }
   { dg-error "non-lvalue|invalid" "array not decaying to lvalue" { target *-*-* } 21 }
   { dg-error "non-lvalue|invalid" "array not decaying to lvalue" { target *-*-* } 22 }
   { dg-error "non-lvalue|invalid" "array not decaying to lvalue" { target *-*-* } 23 }
*/
