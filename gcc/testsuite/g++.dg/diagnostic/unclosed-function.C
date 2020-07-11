void test (void)
{ /* { dg-message "1: to match this '.'" } */
  int filler;
  /* { dg-error "-:expected '.' at end of input" "" { target *-*-* } .+1 } */
