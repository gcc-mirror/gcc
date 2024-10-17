/* Test C2Y octal constants.  Invalid constants.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

int a = 0o; /* { dg-error "invalid suffix" } */
int b = 0Oa; /* { dg-error "invalid suffix" } */
int c = 0O08; /* { dg-error "invalid digit" } */
int d = 0o1.1; /* { dg-error "invalid prefix" } */
int e = 0O0p0; /* { dg-error "invalid suffix" } */
