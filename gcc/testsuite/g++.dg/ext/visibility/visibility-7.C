/* Test warning from conflicting visibility specifications. */
/* { dg-require-visibility "protected" } */
/* { dg-final { scan-hidden "xyzzy" } } */

extern int 
__attribute__((visibility ("hidden")))
xyzzy; /* { dg-message "previous declaration" } */

int 
__attribute__((visibility ("protected")))
xyzzy = 5; /* { dg-warning "visibility attribute ignored" } */
