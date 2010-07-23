/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler-not "cvtss2sd" } } */
float a,b;
main()
{
	return a<0.0;
}
