/* PR 28152: error messages should mention __complex__ */
/* { dg-do compile } */
/* { dg-options "" } */
int
main (void)
{
  __complex__ float z;

  z = __complex__ (1.90000007326203904e+19, 0.0);   // { dg-error "expected primary-expression before '__complex__'" "primary-expression" } 
  // { dg-error "expected .;. before .__complex__." "semicolon" { target *-*-* } 9 } 
  z = __complex__ (1.0e+0, 0.0) / z;    // { dg-error "expected primary-expression before '__complex__'" "primaty-expression" } 
  // { dg-error "expected .;. before '__complex__'" "semicolon" { target *-*-* } 11 } 
  // { dg-error "at end of input" "end" { target *-*-* } 11 } 
