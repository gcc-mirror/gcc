/* PR rtl-optimization/38245 */

struct B { long a, b; char p[32]; };
extern int globv;

extern int b1 (long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long)
     __attribute__((pure, noinline));
extern int b2 (long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long,
	       long long, long, long, long, long, long, long, long)
     __attribute__((const, noinline));
extern int b3 (long long, long, long, long, long, long, long, struct B,
	       long long, struct B, long, struct B, long, long, long, long,
	       long long, struct B, long, struct B, long, long, long, long,
	       long long, struct B, long, struct B, long, long, long, long)
     __attribute__((pure, noinline));
extern int b4 (long long, long, long, long, long, long, long, struct B,
	       long long, struct B, long, struct B, long, long, long, long,
	       long long, struct B, long, struct B, long, long, long, long,
	       long long, struct B, long, struct B, long, long, long, long)
     __attribute__((const, noinline));
extern int b5 () __attribute__((pure, noinline));
extern int b6 () __attribute__((const, noinline));
extern int b7 (int, int)
     __attribute__((pure, noinline));
extern int b8 (int, int)
     __attribute__((const, noinline));
extern int b9 (int, int, int, int, int, int, int)
     __attribute__((pure, noinline));
extern int b10 (int, int, int, int, int, int, int)
     __attribute__((const, noinline));
