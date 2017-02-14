typedef unsigned int u32;

static inline u32 rot(u32 x, u32 n, u32 mb, u32 me)
{
	u32 y = x;

	if (n)
		x = 0
#ifdef SL
		| (x << n)
#endif
#ifdef SR
		| (x >> (32 - n))
#endif
		;

	u32 s = -1;
	if (n)
		s = 0
#ifdef SL
		| (s << n)
#endif
#ifdef SR
		| (s >> (32 - n))
#endif
		;

	u32 mask = 0;
	mask += 1U << (31 - mb);
	mask += 1U << (31 - mb);
	mask -= 1U << (31 - me);
	mask -= (mb > me);

	if (mask & ~s)
		return 12345*y;

	return x & mask;
}

#define X3(N,B,E) \
u32 f_##N##_##B##_##E(u32 x) { return rot(x,N,B,E); } \

#define X2(N,B) \
X3(N,B,0) \
X3(N,B,1) \
X3(N,B,2) \
X3(N,B,7) \
X3(N,B,8) \
X3(N,B,9) \
X3(N,B,15) \
X3(N,B,16) \
X3(N,B,17) \
X3(N,B,23) \
X3(N,B,24) \
X3(N,B,25) \
X3(N,B,29) \
X3(N,B,30) \
X3(N,B,31)
#define X1(N) \
X2(N,0) \
X2(N,1) \
X2(N,2) \
X2(N,7) \
X2(N,8) \
X2(N,9) \
X2(N,15) \
X2(N,16) \
X2(N,17) \
X2(N,23) \
X2(N,24) \
X2(N,25) \
X2(N,29) \
X2(N,30) \
X2(N,31)
#define X() \
X1(0) \
X1(1) \
X1(2) \
X1(7) \
X1(8) \
X1(9) \
X1(15) \
X1(16) \
X1(17) \
X1(23) \
X1(24) \
X1(25) \
X1(29) \
X1(30) \
X1(31)

X()
