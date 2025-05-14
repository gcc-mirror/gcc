/* { dg-do compile } */
/* { dg-options "-O2 -ffp-contract=off -ftree-vectorize" } */
/* { dg-final { scan-assembler "fpmpy" } } */

/* fpma and fpms will show in either way because there are no
   specific vector add/sub instructions.  So we just check for fpmpy.  */

#define N 16
extern void bar(float *, float *, float *, float *);
void foo()
{
	int i;
	float a[N], b[N], c[N], d[N];
	bar(a,b,c,d);
	for (i = 0; i < N; i++) {
		a[i] = b[i] + c[i] * d[i];
	}
	bar(a,b,c,d);
	for (i = 0; i < N; i++) {
		a[i] = b[i] - c[i] * d[i];
	}
	bar(a,b,c,d);
	for (i = 0; i < N; i++) {
		a[i] = b[i] * c[i] + d[i];
	}
	bar(a,b,c,d);
	for (i = 0; i < N; i++) {
		a[i] = b[i] * c[i] - d[i];
	}
	bar(a,b,c,d);
}
