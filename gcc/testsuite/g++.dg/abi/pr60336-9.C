// { dg-do compile }
// { dg-options "-O2 -std=c++11 -fno-pic" }
// { dg-require-effective-target fpic }

struct A1 {}; struct A2 {};
struct B1 { A1 a; A2 b; }; struct B2 { A1 a; A2 b; };
struct C1 { B1 a; B2 b; }; struct C2 { B1 a; B2 b; };
struct D1 { C1 a; C2 b; }; struct D2 { C1 a; C2 b; };
struct E1 { D1 a; D2 b; }; struct E2 { D1 a; D2 b; };
struct F1 { E1 a; E2 b; }; struct F2 { E1 a; E2 b; };
struct G1 { F1 a; F2 b; }; struct G2 { F1 a; F2 b; };
struct H1 { G1 a; G2 b; }; struct H2 { G1 a; G2 b; };
struct I1 { H1 a; H2 b; }; struct I2 { H1 a; H2 b; };
struct J1 { I1 a; I2 b; }; struct J2 { I1 a; I2 b; };
struct dummy { J1 a; J2 b; };

struct true_type { struct dummy i; };

extern true_type y;
extern void xxx (true_type c);

void
yyy (void)
{
  xxx (y);
}

// { dg-final { scan-assembler "jmp\[\t \]+\[^\$\]*?_Z3xxx9true_type" { target i?86-*-* x86_64-*-* } } }
