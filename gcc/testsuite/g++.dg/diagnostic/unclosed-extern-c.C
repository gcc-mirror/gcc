extern "C" { // { dg-line open_extern_c }

  int foo (void);

/* Missing close-brace for the extern "C" here.  */

template <typename T> // { dg-error "template with C linkage" }
void bar (void);
// { dg-message "1: 'extern .C.' linkage started here" "" { target *-*-* } open_extern_c }

void test (void);
// { message "12: to match this '.'" "" { target *-*-* } open_extern_c }
/* { dg-error "18:expected '.' at end of input" "" { target *-*-* } .-2 } */
