/* Test implicit conversion of register arrays to pointers:
   implementation-defined in C2y, undefined behavior previously.  GCC disallows
   this conversion (the case of array element access changed in C2y no longer
   to involve the implicit conversion to a pointer).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
f ()
{
  register int a[1];
  constexpr register int b[1] = { 1 };
  a; /* { dg-error "address of register variable" } */
  b; /* { dg-error "address of register variable" } */
  (void) a; /* { dg-error "address of register variable" } */
  (void) b; /* { dg-error "address of register variable" } */
  *a; /* { dg-error "address of register variable" } */
  *b; /* { dg-error "address of register variable" } */
  a + 0; /* { dg-error "address of register variable" } */
  b + 0; /* { dg-error "address of register variable" } */
}
