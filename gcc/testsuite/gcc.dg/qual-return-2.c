/* Test for warnings for qualified function return types.  -pedantic test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

/* Qualifying a function return type makes no sense.  */

/* The first two of these shouldn't warn (with just -pedantic) as long
   as the function is not defined.  */

volatile void vvoid_fn (void);
volatile void (*vvoid_ptr) (void);
volatile void vvoid_fn2 (void) { } /* { dg-warning "qualified" "volatile defn" } */
