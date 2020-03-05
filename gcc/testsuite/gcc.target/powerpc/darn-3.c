/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

static int darn32(void) { return __builtin_darn_32(); }

int four(void)
{
	int sum = 0;
	int i;
	for (i = 0; i < 4; i++)
		sum += darn32();
	return sum;
}

/* { dg-final { scan-assembler-times {(?n)\mdarn .*,0\M} 4 } } */
