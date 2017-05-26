/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo()
{
  int i;
  *(long*)&i = 0;  /* { dg-warning "type-punn" "type-punn" } */
  /* These messages are only expected for lp64, but fail there.  When they
     pass for lp64, replace "xfail *-*-*" with "target lp64".  */
  /* { dg-message "does break strict-aliasing" "break" { xfail *-*-* } .-3 } */
  /* { dg-message "initialized" "init" { xfail *-*-* } .-4 } */
  return i;
}
