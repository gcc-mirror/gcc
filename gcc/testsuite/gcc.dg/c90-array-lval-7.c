/* Test for non-lvalue arrays: test that C90 does not allow them in
   conditional expressions, while in C99 they decay and are
   allowed.  */

/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { char c[1]; };
struct s a, b, c;
int d;
int e;

void
bar (void)
{
  /* In C90, the non-lvalue arrays do not decay to pointers, and
     6.3.15 does not permit conditional expressions between arrays.
     In C99, they decay to pointers.  */
  (e ? (d ? b : c).c : (e ? b : c).c); /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "array" "bad conditional" { target *-*-* } .-1 } */
}
