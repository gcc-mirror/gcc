/* Test warning from conflicting visibility specifications. */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

extern int 
__attribute__((visibility ("hidden")))
xyzzy; /* { dg-warning "previous declaration here" "" } */

int 
__attribute__((visibility ("protected")))
xyzzy = 5; /* { dg-warning "visibility attribute ignored" "" } */
