/* { dg-do compile } */
#include <altivec.h>
void valuen014(vector float p1, vector float p2, vector float p3,
	       vector float p4, vector float p5, vector float p6,
	       vector float p7, vector float p8, vector float p9,
	       vector float p10, vector float p11, vector float p12,
	       int p13)
{
}

void f()
{
  valuen014(((vector float) {1.83e+09, 5.73e+08, -2.96e+08, -7.46e+08}),
	    ((vector float) {-2.01e+09, 9.89e+08, -1.92e+09, 2.09e+09}),
	    ((vector float) {1.95e+09, -2.41e+08, 2.67e+08, 1.67e+09}),
	    ((vector float) {-2.12e+09, 8.18e+08, 9.47e+08, -1.25e+09}),
	    ((vector float) {-9.47e+08, -9.3e+08, -1.65e+09, 1.64e+09}),
	    ((vector float) {-7.99e+07, 4.86e+08, -3.4e+06, 3.11e+08}),
	    ((vector float) {1.78e+09, 1.22e+09, -1.27e+09, -3.11e+08}),
	    ((vector float) {1.41e+09, -5.38e+07, -2.08e+09, 1.54e+09}),
	    ((vector float) {3.1e+08, -1.49e+09, 5.38e+08, -1.3e+09}),
	    ((vector float) {9.66e+08, 5.5e+08, 1.75e+08, -8.22e+07}),
	    ((vector float) {-1.72e+08, -2.06e+09, 1.14e+09, -4.64e+08}),
	    ((vector float) {-1.25e+09, 8.12e+07, -2.02e+09, 4.71e+08}), 
	    962425441);
}
