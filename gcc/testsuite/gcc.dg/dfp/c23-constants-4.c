/* Test that DFP constants are accepted in C23 mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

int a = (int) 1.1D32; /* { dg-warning "C23 feature" } */
int b = (int) 2.d32; /* { dg-warning "C23 feature" } */
int c = (int) .33D64; /* { dg-warning "C23 feature" } */
int d = (int) 2e1d64; /* { dg-warning "C23 feature" } */
int e = (int) .3e2D128; /* { dg-warning "C23 feature" } */
int f = (int) 4.5e3d128; /* { dg-warning "C23 feature" } */
int g = (int) 5.e0D32; /* { dg-warning "C23 feature" } */
int h = (int) 1e+2d32; /* { dg-warning "C23 feature" } */
int i = (int) 1000e-3D128; /* { dg-warning "C23 feature" } */
