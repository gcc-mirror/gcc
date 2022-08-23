// PR c++/96604

struct A { template<typename T> operator T(); };
struct X {};
struct B { friend A::operator X(); };
