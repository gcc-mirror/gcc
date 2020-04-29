// PR target/94707
// { dg-do compile { target powerpc*-*-darwin* } }
// { dg-require-effective-target ilp32 }
// { dg-options "-std=c++14" }

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
static_assert (__alignof__ (A) == 8, "");
static_assert (__alignof__ (B) == 8, "");
static_assert (__alignof__ (C) == 8, "");
static_assert (__alignof__ (D) == 4, "");
static_assert (__alignof__ (E) == 4, "");
static_assert (__alignof__ (F) == 8, "");
static_assert (__alignof__ (G) == 8, "");
static_assert (__alignof__ (H) == 4, "");
static_assert (__alignof__ (I) == 4, "");
static_assert (__alignof__ (J) == 8, "");
static_assert (__alignof__ (K) == 8, "");
static_assert (__alignof__ (L) == 8, "");
static_assert (__alignof__ (M) == 8, "");
