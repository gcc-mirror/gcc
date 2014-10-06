/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler-not "cvtss2sd" } } */
extern double fabs (double);
float a,b;
int
main()
{
	a=fabs(b)+1.0;
}
