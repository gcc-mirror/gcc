void test (void)
{ /* { dg-message "1: to match this '.'" } */
  int filler; /* { dg-error "13: expected '.' at end of input" } */
