// { dg-do compile { target c++11 } }

struct A { int i; };
struct B: A { constexpr B(): A{} {} };
struct B2: A { constexpr B2(): A{1} {} };

struct C { protected: int i; };
struct D: C { constexpr D(): C{} {} };
