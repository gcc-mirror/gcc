/* Test warning from conflicting visibility specifications. */
/* { dg-do compile } */
/* { dg-require-visibility "protected" } */
/* { dg-final { scan-hidden "xyzzy" } } */

extern int 
__attribute__((visibility ("hidden")))
xyzzy; /* { dg-warning "previous declaration" "" } */

int 
__attribute__((visibility ("protected")))
xyzzy = 5; /* { dg-warning "different visibility" "" } */
