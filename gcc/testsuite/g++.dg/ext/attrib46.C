// PR c++/40821

struct __attribute__((aligned(8)) S1 { int i; }; // { dg-error "expected" }
struct __attribute__( aligned(8)  S2 { int i; }; // { dg-error "expected" }
