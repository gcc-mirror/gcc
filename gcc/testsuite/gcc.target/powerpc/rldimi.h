typedef unsigned int u32;
typedef unsigned long long u64;

static inline u64 rot_insert(u64 x, u64 y, u32 n, u32 mb, u32 me)
{
	if (n)
		x = 0
#ifdef SL
		| (x << n)
#endif
#ifdef SR
		| (x >> (64 - n))
#endif
		;

	u64 s = -1;
	if (n)
		s = 0
#ifdef SL
		| (s << n)
#endif
#ifdef SR
		| (s >> (64 - n))
#endif
		;

	u64 mask = 0;
	mask += 1ULL << (63 - mb);
	mask += 1ULL << (63 - mb);
	mask -= 1ULL << (63 - me);
	mask -= (mb > me);

	if (mask & ~s)
		return 12345*y;

	return (x & mask) | (y & ~mask);
}

#define X2(N,B) \
u64 f_##N##_##B(u64 x, u64 y) { return rot_insert(x,y,N,B,63-N); } \
u64 g_##N##_##B(u64 x, u64 y) { return rot_insert(y,x,N,B,63-N); }

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
X2(N,31) \
X2(N,32) \
X2(N,33) \
X2(N,34) \
X2(N,39) \
X2(N,40) \
X2(N,41) \
X2(N,47) \
X2(N,48) \
X2(N,49) \
X2(N,55) \
X2(N,56) \
X2(N,57) \
X2(N,61) \
X2(N,62) \
X2(N,63)
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
X1(31) \
X1(32) \
X1(33) \
X1(34) \
X1(39) \
X1(40) \
X1(41) \
X1(47) \
X1(48) \
X1(49) \
X1(55) \
X1(56) \
X1(57) \
X1(61) \
X1(62) \
X1(63)

X()
