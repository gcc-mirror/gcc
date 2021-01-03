/* Test C2x binary constants.  Invalid constants.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

int a = 0b; /* { dg-error "invalid suffix" } */
int b = 0B2; /* { dg-error "invalid suffix" } */
int c = 0B02; /* { dg-error "invalid digit" } */
int d = 0b1.1; /* { dg-error "invalid prefix" } */
int e = 0B0p0; /* { dg-error "invalid suffix" } */
