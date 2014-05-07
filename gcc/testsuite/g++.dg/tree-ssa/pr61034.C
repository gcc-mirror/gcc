// { dg-do compile }
// { dg-options "-O3 -fdump-tree-fre2" }

#define assume(x) if(!(x))__builtin_unreachable()

inline void* operator new(__SIZE_TYPE__ n){ return __builtin_malloc(n); }
inline void operator delete(void *p) { __builtin_free(p); }
struct O {
    double num;
    int count;
};
struct I {
    O *o;
    I(double d = 0) : o (new O) { o->num = d; o->count = 1; }
    I(I const&i) { assume(i.o->count >= 1); o = i.o; ++o->count; }
    I& operator=(I const&i) { I(i).swap(*this); return *this; }
    ~I() { if (--o->count == 0) delete o; }
    void swap(I& i) { O *tmp = o; o = i.o; i.o = tmp; }
    I& operator*= (I const&i) {
	if (o->count > 1) *this = I(o->num);
	o->num *= i.o->num;
	return *this;
    }
    I& operator-= (I const&i) {
	if (o->count > 1) *this = I(o->num);
	o->num -= i.o->num;
	return *this;
    }
};
inline I operator* (I a, I const&b) { return a *= b; }
inline I operator- (I a, I const&b) { return a -= b; }
inline bool operator< (I const&a, I const&b) { return a.o->num < b.o->num; }

bool f(I a, I b, I c, I d) {
    return (a * d - b * c) * (a * b - c * d) < 42;
}

// We should be able to CSE most references to count and thus remove
// a bunch of conditional free()s and unreachable()s.
// This works only if everything is inlined into 'f'.

// { dg-final { scan-tree-dump-times ";; Function" 1 "fre2" } }
// { dg-final { scan-tree-dump-times "free" 19 "fre2" } }
// { dg-final { scan-tree-dump-times "unreachable" 11 "fre2" } }
// { dg-final { cleanup-tree-dump "fre2" } }
