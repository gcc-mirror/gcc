/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2" } */

unsigned int functest(unsigned int x)
{
	return __builtin_ctz(x);
}

/* { dg-final { scan-assembler "rbit" } } */
/* { dg-final { scan-assembler "clz" } } */
/* { dg-final { scan-assembler-not "rsb" } } */
