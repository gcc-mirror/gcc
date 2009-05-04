/* PR 28152: error messages should mention _Complex */
/* { dg-do compile } */
/* { dg-options "" } */
int
main (void)
{
  _Complex float z;

  z = _Complex (1.90000007326203904e+19, 0.0);   // { dg-error "expected primary-expression before '_Complex'" } 
  // { dg-error "expected .;. before ._Complex." "" { target *-*-* } 9 } 
  z = _Complex (1.0e+0, 0.0) / z;    // { dg-error "expected primary-expression before '_Complex'" } 
  // { dg-error "expected .;. before '_Complex'" "" { target *-*-* } 11 } 
  // { dg-error "at end of input" "" { target *-*-* } 11 } 
