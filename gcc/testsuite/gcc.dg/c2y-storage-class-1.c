/* Test C2y constraint on storage-class specifiers for block-scope identifiers
   for functions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
f ()
{
  auto void g1 (); /* { dg-error "invalid storage class for function" } */
  /* { dg-error "nested function" "nested" { target *-*-* } .-1 } */
  constexpr void g2 (); /* { dg-error "requires an initialized data declaration" } */
  extern void g3 ();
  register void g4 (); /* { dg-error "invalid storage class for function" } */
  static void g5 (); /* { dg-error "invalid storage class for function" } */
  thread_local void g6 (); /* { dg-error "implicitly auto and declared" } */
  static thread_local void g7 (); /* { dg-error "invalid storage class for function" } */
  /* { dg-error "nested function" "nested" { target *-*-* } .-1 } */
}
