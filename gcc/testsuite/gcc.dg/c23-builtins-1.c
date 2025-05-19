/* Test C23 built-in functions: test functions new in C23 are indeed
   declared as built-in as expected.  Non-DFP tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

/* Keep this list sorted alphabetically by function name.  */
int acospi (void); /* { dg-warning "conflicting types for built-in function" } */
int acospif (void); /* { dg-warning "conflicting types for built-in function" } */
int acospil (void); /* { dg-warning "conflicting types for built-in function" } */
int asinpi (void); /* { dg-warning "conflicting types for built-in function" } */
int asinpif (void); /* { dg-warning "conflicting types for built-in function" } */
int asinpil (void); /* { dg-warning "conflicting types for built-in function" } */
int atan2pi (void); /* { dg-warning "conflicting types for built-in function" } */
int atan2pif (void); /* { dg-warning "conflicting types for built-in function" } */
int atan2pil (void); /* { dg-warning "conflicting types for built-in function" } */
int atanpi (void); /* { dg-warning "conflicting types for built-in function" } */
int atanpif (void); /* { dg-warning "conflicting types for built-in function" } */
int atanpil (void); /* { dg-warning "conflicting types for built-in function" } */
int cospi (void); /* { dg-warning "conflicting types for built-in function" } */
int cospif (void); /* { dg-warning "conflicting types for built-in function" } */
int cospil (void); /* { dg-warning "conflicting types for built-in function" } */
int exp10 (void); /* { dg-warning "conflicting types for built-in function" } */
int exp10f (void); /* { dg-warning "conflicting types for built-in function" } */
int exp10l (void); /* { dg-warning "conflicting types for built-in function" } */
int roundeven (void); /* { dg-warning "conflicting types for built-in function" } */
int roundevenf (void); /* { dg-warning "conflicting types for built-in function" } */
int roundevenl (void); /* { dg-warning "conflicting types for built-in function" } */
int sinpi (void); /* { dg-warning "conflicting types for built-in function" } */
int sinpif (void); /* { dg-warning "conflicting types for built-in function" } */
int sinpil (void); /* { dg-warning "conflicting types for built-in function" } */
int strdup (void); /* { dg-warning "conflicting types for built-in function" } */
int strndup (void); /* { dg-warning "conflicting types for built-in function" } */
int tanpi (void); /* { dg-warning "conflicting types for built-in function" } */
int tanpif (void); /* { dg-warning "conflicting types for built-in function" } */
int tanpil (void); /* { dg-warning "conflicting types for built-in function" } */
