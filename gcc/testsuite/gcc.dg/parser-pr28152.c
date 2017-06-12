/* PR 28152: error messages should mention _Complex */
/* { dg-do compile } */
/* { dg-options "" } */
int
main (void)
{
  _Complex float z;

  z = _Complex (1.90000007326203904e+19, 0.0);   /* { dg-error "_Complex" } */
  z = _Complex (1.0e+0, 0.0) / z;   /* { dg-error "_Complex" "_Complex" } */
  /* { dg-error "at end of input" "end of input" { target *-*-* } .-1 } */
