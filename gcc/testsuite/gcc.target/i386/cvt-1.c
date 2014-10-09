/* { dg-do compile } */
/* { dg-options "-O2 -march=k8 -mfpmath=sse" } */
/* { dg-final { scan-assembler "cvttsd2si" } } */
/* { dg-final { scan-assembler "cvttss2si" } } */
int a,a1;
double b;
float b1;
void
t()
{
	a=b;
	a1=b1;
}
