// PR target/94707
// { dg-do compile { target powerpc_elfv2 } }
// { dg-options "-O2 -std=c++17" }
// Test that for no calls in this testcase the C++17 empty base
// artificial fields and [[no_unique_address]] empty class non-static
// data members are ignored in the decision about passing homogeneous
// arguments.
// { dg-final { scan-assembler-not {(?n)^\s+lfs\s+(?:%f)?4,} } }

struct X { };
struct Y { int : 0; };
struct Z { int : 0; Y y; };
struct U : public X { X q; };
struct A { float a, b, c, d; };
struct B : public X { float a, b, c, d; };
struct C : public Y { float a, b, c, d; };
struct D : public Z { float a, b, c, d; };
struct E : public U { float a, b, c, d; };
struct F { [[no_unique_address]] X x; float a, b, c, d; };
struct G { [[no_unique_address]] Y y; float a, b, c, d; };
struct H { [[no_unique_address]] Z z; float a, b, c, d; };
struct I { [[no_unique_address]] U u; float a, b, c, d; };
struct J { float a, b; [[no_unique_address]] X x; float c, d; };
struct K { float a, b; [[no_unique_address]] Y y; float c, d; };
struct L { float a, b; [[no_unique_address]] Z z; float c, d; };
struct M { float a, b; [[no_unique_address]] U u; float c, d; };
#define T(S, s) extern S s; extern void foo##s (S); int bar##s () { foo##s (s); return 0; }
// { dg-bogus "parameter passing for argument of type" }
T (D, d)
T (E, e)
T (H, h)
T (I, i)
T (L, l)
T (M, m)
