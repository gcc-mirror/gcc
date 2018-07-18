/* { dg-do compile } */
/* { dg-options "-O3 -m32" } */
long long test_and(long long x) {
	return x & 0x77ffffffffULL;
}
/* { dg-final { scan-assembler-not "xmm" } } */
