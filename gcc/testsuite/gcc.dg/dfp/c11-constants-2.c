/* Test that DFP constants are diagnosed in C11 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int a = (int) 1.1DF; /* { dg-error "C2X feature" } */
int b = (int) 2.df; /* { dg-error "C2X feature" } */
int c = (int) .33DD; /* { dg-error "C2X feature" } */
int d = (int) 2e1dd; /* { dg-error "C2X feature" } */
int e = (int) .3e2DL; /* { dg-error "C2X feature" } */
int f = (int) 4.5e3dl; /* { dg-error "C2X feature" } */
int g = (int) 5.e0DF; /* { dg-error "C2X feature" } */
int h = (int) 1e+2df; /* { dg-error "C2X feature" } */
int i = (int) 1000e-3DL; /* { dg-error "C2X feature" } */
