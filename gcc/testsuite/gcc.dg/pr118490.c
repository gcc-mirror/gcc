/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -frounding-math -Wlogical-op" } */

double exp(double);
int foo(int v) {
	return v && exp(1.) < 2.;
}
