// Testcase from P0136
// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct A { A(int); };
struct B : A { using A::A; };

struct C1 : B { using B::B; };
struct C2 : B { using B::B; };

struct D1 : C1, C2 {
  using C1::C1;
  using C2::C2;
};

struct V1 : virtual B { using B::B; };
struct V2 : virtual B { using B::B; };

struct D2 : V1, V2 {
  using V1::V1;
  using V2::V2;
};

D1 d1(0); // { dg-error "" } ambiguous
D2 d2(0); // OK: initializes virtual B base class, which initializes the A base
          // class then initializes the V1 and V2 base classes as if by a
          // defaulted default constructor
