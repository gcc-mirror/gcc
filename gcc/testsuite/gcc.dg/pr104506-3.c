/* { dg-do compile } */
/* PR c/104506: we used to ICE after the error of
   changing the type.  */
double x;
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } .-1 } */
void
foo (void)
{
  x;
}
int x; /* { dg-error "conflicting types" } */
