/* Test for warnings for qualified function return types.  -pedantic test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

/* Qualifying a function return type makes no sense.  */

/* "volatile void" is a GNU extension, so only warn at -pedantic.
   Strictly, the first two of these should warn only if the function is
   somewhere used or defined.  */

volatile void vvoid_fn (void); /* { dg-warning "qualified" "volatile decl" } */
volatile void (*vvoid_ptr) (void); /* { dg-warning "qualified" "volatile ptr" } */
volatile void vvoid_fn2 (void) { } /* { dg-warning "qualified" "volatile defn" } */
