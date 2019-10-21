/* { dg-do compile } */
/* { dg-options "-O2 -mvsx" } */

vector int c, a, b;

static inline void __attribute__ ((__always_inline__, target ("no-vsx")))
foo () /* { dg-error "inlining failed in call to .* target specific option mismatch" } */
{
  c = a + b;
}

int
main ()
{
  foo (); /* { dg-message "called from here" } */
  c = a + b;
}
