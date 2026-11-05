/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "fld\\.d" } } */
/* { dg-final { scan-assembler-not "movgr2fr" } } */

extern long T[1024];

double test (int i, double n)
{
	return T[i] * n;
}
