static int foo (void) { return 0; } /* { dg-error "weak declaration of 'foo' being applied to a already existing, static definition" } */
int foo (void)  __attribute__((weak));

