void test (void)
{ /* { dg-message "1: to match this '.'" } */
  int filler; /* { dg-error "14:expected '.' at end of input"  } */
