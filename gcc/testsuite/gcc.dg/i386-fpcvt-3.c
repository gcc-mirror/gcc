/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler-not "cvtss2sd" } } */
extern double fabs (double);
float a,b;
main()
{
	a=fabs(b)+1.0;
}
