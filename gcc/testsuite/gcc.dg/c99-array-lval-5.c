/* Test for non-lvalue arrays: test that the unary '&' operator is not
   allowed on them, for both C90 and C99.  */

/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s { char c[1]; };

extern struct s foo (void);
struct s a, b, c;
int d;

void
bar (void)
{
  &((foo ()).c); /* { dg-bogus "warning" "warning in place of error" } */
  &((d ? b : c).c); /* { dg-bogus "warning" "warning in place of error" } */
  &((d, b).c); /* { dg-bogus "warning" "warning in place of error" } */
  &((a = b).c); /* { dg-bogus "warning" "warning in place of error" } */
}
/* { dg-error "lvalue" "bad address-of" { target *-*-* } 17 }
   { dg-error "lvalue" "bad address-of" { target *-*-* } 18 }
   { dg-error "lvalue" "bad address-of" { target *-*-* } 19 }
   { dg-error "lvalue" "bad address-of" { target *-*-* } 20 }
*/
