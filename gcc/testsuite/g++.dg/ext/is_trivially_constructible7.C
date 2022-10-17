// PR c++/102535
// Verify __is_trivially_constructible works with multi-arg paren init of
// aggrs.

struct A { int x; };
struct B { float y; };
struct C { char z; };
struct D { A a; B b; C c; };

extern int n[1 + __is_trivially_constructible(D, A)];
extern int n[1 + __is_trivially_constructible(D, A, B)];
extern int n[1 + __is_trivially_constructible(D, A, B, C)];
#if __cpp_aggregate_paren_init
extern int n[1 + true];
#else
extern int n[1 + false];
#endif
