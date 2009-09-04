/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -march=armv6t2" } */

unsigned int functest(unsigned int x)
{
	return __builtin_ctz(x);
}

/* { dg-final { scan-assembler "rbit" } } */
/* { dg-final { scan-assembler "clz" } } */
/* { dg-final { scan-assembler-not "rsb" } } */
