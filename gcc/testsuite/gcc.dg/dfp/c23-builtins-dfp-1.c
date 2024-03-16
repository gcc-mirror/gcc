/* Test C23 built-in functions: test functions new in C23 are indeed
   declared as built-in as expected.  DFP tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

int fabsd32 (void); /* { dg-warning "conflicting types for built-in function" } */
int fabsd64 (void); /* { dg-warning "conflicting types for built-in function" } */
int fabsd128 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand32 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand64 (void); /* { dg-warning "conflicting types for built-in function" } */
int nand128 (void); /* { dg-warning "conflicting types for built-in function" } */
