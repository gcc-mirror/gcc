/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "sbb" } } */

int
t(float a, float b)
{
	return a<=b?0:-1;
}
