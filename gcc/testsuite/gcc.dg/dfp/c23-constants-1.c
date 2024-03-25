/* Test that DFP constants are accepted in C23 mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a = (int) 1.1DF;
int b = (int) 2.df;
int c = (int) .33DD;
int d = (int) 2e1dd;
int e = (int) .3e2DL;
int f = (int) 4.5e3dl;
int g = (int) 5.e0DF;
int h = (int) 1e+2df;
int i = (int) 1000e-3DL;
