/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512vl" } */
/* { dg-final { scan-assembler "vinserti128" } } */
/* { dg-final { scan-assembler-not "vinserti32x4" } } */

void sink(int *);

void f(const unsigned *tab, unsigned long long k)
{
	int out[8];
	out[0] = tab[(k >>  0) & 15];
	out[1] = tab[(k >>  4) & 15];
	out[2] = tab[(k >>  8) & 15];
	out[3] = tab[(k >> 12) & 15];
	out[4] = tab[(k >> 16) & 15];
	out[5] = tab[(k >> 20) & 15];
	out[6] = tab[(k >> 24) & 15];
	out[7] = tab[(k >> 28) & 15];
	sink(out);
}
