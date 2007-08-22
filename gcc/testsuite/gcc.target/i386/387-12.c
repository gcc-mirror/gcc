/* PR target/26915 */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O" } */

double testm0(void)
{
	return -0.0;
}

double testm1(void)
{
	return -1.0;
}

/* { dg-final { scan-assembler "fldz" } } */
/* { dg-final { scan-assembler "fld1" } } */
