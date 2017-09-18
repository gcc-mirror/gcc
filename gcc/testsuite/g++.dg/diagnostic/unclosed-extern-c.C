extern "C" { /* { dg-message "12: to match this '.'" } */

void test (void); /* { dg-error "17: expected '.' at end of input" } */
