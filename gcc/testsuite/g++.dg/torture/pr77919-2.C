// PR rtl-optimization/77919
// { dg-do compile }

typedef _Complex long long B;
struct A { A (double) {} _Complex double i; };
typedef struct { B b; } C;
struct D { D (const B &x) : b (x) {} B b; };
static inline B foo (const double *x) { C *a; a = (C *) x; return a->b; }
static inline D baz (const A &x) { return foo ((double *) &x); }
D b = baz (0);
