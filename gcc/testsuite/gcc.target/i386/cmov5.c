/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "sbb" } } */

int
t(float a, float b)
{
	return a<=b?0:-1;
}
