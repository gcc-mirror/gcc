/* { dg-require-effective-target int32plus } */

/* Failed on sparc with -mv8plus because sparc.c:set_extends() thought
   erroneously that SImode ASHIFT chops the upper bits, it does not.  */

typedef unsigned long long uint64;

void g(uint64 x, int y, int z, uint64 *p)
{
	unsigned w = ((x >> y) & 0xffffffffULL) << (z & 0x1f);
	*p |= (w & 0xffffffffULL) << z;
}

int main(void)
{
	uint64 a = 0;
	g(0xdeadbeef01234567ULL, 0, 0, &a);
	return (a == 0x01234567) ? 0 : 1;
}



