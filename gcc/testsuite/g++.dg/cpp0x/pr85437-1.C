// PR c++/85437
// { dg-do compile { target c++11 } }

struct A { int a; constexpr A() : a(0) {} };
struct B : A { int x; constexpr B() : x(0) {} };
struct X { int z; constexpr X() : z(0) {} };
struct C : X, B {};
constexpr int C::*cbx = &B::x;
constexpr int B::*bx = &B::x;
constexpr int A::*abx = static_cast<int(A::*)>(&B::x);	// { dg-bogus "not a constant expression" }

constexpr const C y;
constexpr const B& yb = y;
constexpr const A& ya = y;
constexpr int const *pcbx = &(y.*cbx);
constexpr int const *pbx = &(y.*bx);
constexpr int const *pabx = &(ya.*abx);
