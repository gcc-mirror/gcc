/* { dg-require-effective-target indirect_calls } */

typedef long		__kernel_time_t;
typedef __kernel_time_t		time_t;
time2(
    void (* const  (funcp)(time_t)),
    const long offset, int * const okayp)
{
	register int			bits;
	time_t				t;
	for (bits = 0, t = 1; t > 0; ++bits, t <<= 1)
		;
	t = (t < 0) ? 0 : ((time_t) 1 << bits);
	for ( ; ; ) {
		(*funcp)((time_t)&t);
	}
}
