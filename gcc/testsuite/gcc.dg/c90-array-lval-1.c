/* Test for non-lvalue arrays decaying to pointers: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { char c[1]; };

extern struct s foo (void);

void
bar (void)
{
  char *t;
  (foo ()).c[0]; /* { dg-bogus "warning" "warning in place of error" } */
  t = (foo ()).c; /* { dg-bogus "warning" "warning in place of error" } */
  (foo ()).c + 1; /* { dg-bogus "warning" "warning in place of error" } */
}
/* { dg-error "non-lvalue" "array not decaying to lvalue" { target *-*-* } 14 }
   { dg-error "non-lvalue|incompatible" "array not decaying to lvalue" { target *-*-* } 15 }
   { dg-error "non-lvalue|invalid" "array not decaying to lvalue" { target *-*-* } 16 }
*/
