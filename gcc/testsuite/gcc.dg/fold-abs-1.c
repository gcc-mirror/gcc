/* { dg-do compile } */
/* { dg-options "-O1 -fwrapv" } */
#define ABS(x) (x > 0 ? x : -x)
int f (int a, int b) {
	if ((ABS(a) | b) != 0) return 1;
	else return 0;
}
