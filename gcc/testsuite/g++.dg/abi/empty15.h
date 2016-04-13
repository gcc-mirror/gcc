struct A1 {};
struct A2 {};
struct B1 { struct A1 a; struct A2 b; };
struct B2 { struct A1 a; struct A2 b; };
struct C1 { struct B1 a; struct B2 b; };
struct C2 { struct B1 a; struct B2 b; };
struct D1 { struct C1 a; struct C2 b; };
struct D2 { struct C1 a; struct C2 b; };
struct E1 { struct D1 a; struct D2 b; };
struct E2 { struct D1 a; struct D2 b; };
struct F1 { struct E1 a; struct E2 b; };
struct F2 { struct E1 a; struct E2 b; };
struct G1 { struct F1 a; struct F2 b; };
struct G2 { struct F1 a; struct F2 b; };
struct H1 { struct G1 a; struct G2 b; };
struct H2 { struct G1 a; struct G2 b; };
struct I1 { struct H1 a; struct H2 b; };
struct I2 { struct H1 a; struct H2 b; };
struct J1 { struct I1 a; struct I2 b; };
struct J2 { struct I1 a; struct I2 b; };
struct dummy { struct J1 a; struct J2 b; };

struct foo
{
  int i1;
  int i2;
  int i3;
  int i4;
  int i5;
};
