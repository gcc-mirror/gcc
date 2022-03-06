/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */
/* PR c/104506: we used to ICE after the error of
   changing the type.  */

void
foo (double x)
/* { dg-message "note: previous definition" "previous definition" { target *-*-* } .-1 } */
{
  (void)x;
  int x; /* { dg-error "redeclared as different kind of symbol" } */
}
