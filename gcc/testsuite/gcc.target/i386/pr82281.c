/* PR target/82281 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mtune=znver1" } */

long long test_and(long long x) {
	return x & 0x77ffffffffULL;
}

/* { dg-final { scan-assembler-not "xmm" } } */
