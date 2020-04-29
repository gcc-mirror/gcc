// PR target/94704
// { dg-do compile }
// { dg-options "-O2 -std=c++17" }
// Test that for all the calls in this testcase the C++17 empty base
// artificial fields and [[no_unique_address]] empty class non-static
// data members are ignored in the decision whether passed arguments
// have a single floating point field.
// { dg-final { scan-assembler-times {(?n)^\s+ld\s+%f0,} 7 } }

struct X { };
struct Y { int : 0; };
struct Z { int : 0; Y y; };
struct U : public X { X q; };
struct A { double a; };
struct B : public X { double a; };
struct C : public Y { double a; };
struct D : public Z { double a; };
struct E : public U { double a; };
struct F { [[no_unique_address]] X x; double a; };
struct G { [[no_unique_address]] Y y; double a; };
struct H { [[no_unique_address]] Z z; double a; };
struct I { [[no_unique_address]] U u; double a; };
struct J { double a; [[no_unique_address]] X x; };
struct K { double a; [[no_unique_address]] Y y; };
struct L { double a; [[no_unique_address]] Z z; };
struct M { double a; [[no_unique_address]] U u; };
#define T(S, s) extern S s; extern void foo##s (S); int bar##s () { foo##s (s); return 0; }
// { dg-message "parameter passing for argument of type 'B' when C\\+\\+17 is enabled changed to match C\\+\\+14 in GCC 10.1" "" { target *-*-* } .-1 }
// { dg-message "parameter passing for argument of type 'C' when C\\+\\+17 is enabled changed to match C\\+\\+14 in GCC 10.1" "" { target *-*-* } .-2 }
// { dg-message "parameter passing for argument of type 'F' with '\\\[\\\[no_unique_address\\\]\\\]' members changed in GCC 10.1" "" { target *-*-* } .-3 }
// { dg-message "parameter passing for argument of type 'G' with '\\\[\\\[no_unique_address\\\]\\\]' members changed in GCC 10.1" "" { target *-*-* } .-4 }
// { dg-message "parameter passing for argument of type 'J' with '\\\[\\\[no_unique_address\\\]\\\]' members changed in GCC 10.1" "" { target *-*-* } .-5 }
// { dg-message "parameter passing for argument of type 'K' with '\\\[\\\[no_unique_address\\\]\\\]' members changed in GCC 10.1" "" { target *-*-* } .-6 }
T (A, a)
T (B, b)
T (C, c)
T (F, f)
T (G, g)
T (J, j)
T (K, k)
