/* Test C23 built-in functions: test functions new in C23 are indeed
   declared as built-in as expected.  Non-DFP tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

int exp10 (void); /* { dg-warning "conflicting types for built-in function" } */
int exp10f (void); /* { dg-warning "conflicting types for built-in function" } */
int exp10l (void); /* { dg-warning "conflicting types for built-in function" } */
int roundeven (void); /* { dg-warning "conflicting types for built-in function" } */
int roundevenf (void); /* { dg-warning "conflicting types for built-in function" } */
int roundevenl (void); /* { dg-warning "conflicting types for built-in function" } */
int strdup (void); /* { dg-warning "conflicting types for built-in function" } */
int strndup (void); /* { dg-warning "conflicting types for built-in function" } */
