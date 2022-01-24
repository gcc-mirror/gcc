// PR c++/103012
// { dg-do compile }
// { dg-options "-mno-avx2" }

int a = 1;
#pragma GCC target "avx2"
#define A(a) a +
#define B(a) A(a)A(a)
#define C(a) B(a)B(a)
#define D(a) C(a)C(a)
#define E(a) D(a)D(a)
#define F(a) E(a)E(a)
#define G(a) F(a)F(a)
#define H(a) G(a)G(a)
#define I(a) H(a)H(a)
#define J(a) I(a)I(a)
#define K(a) J(a)J(a)
#define L(a) K(a)K(a)
int b = L(a) 1;
