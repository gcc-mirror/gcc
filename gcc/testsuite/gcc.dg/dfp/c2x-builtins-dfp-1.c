/* Test C2x built-in functions: test functions new in C2x are indeed
   declared as built-in as expected.  DFP tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

int fabsd32 (void); /* { dg-warning "conflicting types for built-in function" } */
int fabsd64 (void); /* { dg-warning "conflicting types for built-in function" } */
int fabsd128 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand32 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand64 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand128 (void); /* { dg-warning "conflicting types for built-in function" } */
