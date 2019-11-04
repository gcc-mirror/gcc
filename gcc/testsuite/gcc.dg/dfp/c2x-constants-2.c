/* Test that DFP constants are accepted in C2X mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat" } */

int a = (int) 1.1DF; /* { dg-warning "C2X feature" } */
int b = (int) 2.df; /* { dg-warning "C2X feature" } */
int c = (int) .33DD; /* { dg-warning "C2X feature" } */
int d = (int) 2e1dd; /* { dg-warning "C2X feature" } */
int e = (int) .3e2DL; /* { dg-warning "C2X feature" } */
int f = (int) 4.5e3dl; /* { dg-warning "C2X feature" } */
int g = (int) 5.e0DF; /* { dg-warning "C2X feature" } */
int h = (int) 1e+2df; /* { dg-warning "C2X feature" } */
int i = (int) 1000e-3DL; /* { dg-warning "C2X feature" } */
