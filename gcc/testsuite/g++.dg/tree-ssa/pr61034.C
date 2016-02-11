// { dg-do compile }
// { dg-options "-O2 -fdump-tree-fre3 -fdump-tree-optimized" }

#define assume(x) if(!(x))__builtin_unreachable()

inline void* operator new(__SIZE_TYPE__ n){ return __builtin_malloc(n); }
inline void operator delete(void *p) { __builtin_free(p); }
// C++14 sized deallocation function
inline void operator delete(void *p, __SIZE_TYPE__) { __builtin_free(p); }
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
    I tmp = (a * d - b * c) * (a * b - c * d);
    return tmp < 42;
}

// We should be able to CSE most references to count and thus remove
// a bunch of conditional free()s and unreachable()s.
// This works only if everything is inlined into 'f'.

// { dg-final { scan-tree-dump-times ";; Function" 1 "fre3" } }
// { dg-final { scan-tree-dump-times "unreachable" 11 "fre3" } }

// Note that depending on PUSH_ARGS_REVERSED we are presented with
// a different initial CFG and thus the final outcome is different

// { dg-final { scan-tree-dump-times "free" 10 "fre3" { target x86_64-*-* i?86-*-* } } }
// { dg-final { scan-tree-dump-times "free" 14 "fre3" { target aarch64-*-* ia64-*-* arm-*-* hppa*-*-* sparc*-*-* powerpc*-*-* alpha*-*-* } } }
// { dg-final { scan-tree-dump-times "free" 0 "optimized" } }
