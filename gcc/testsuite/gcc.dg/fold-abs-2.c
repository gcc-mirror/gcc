/* { dg-do run } */
/* { dg-options "-O1 -fwrapv" } */
#include <limits.h>
void exit (int);
void abort ();
#define ABS(x) (x > 0 ? x : -x)
int f (int a) {
	if (ABS(a) >= 0) return 1;
	else return 0;
}

int main (int argc, char *argv[]) {
	if (f(INT_MIN))
	  abort ();
	else
	  exit (0);
}
