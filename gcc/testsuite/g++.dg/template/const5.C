// PR c++/49176
// { dg-options -std=c++0x }

struct A { static int a(); };

template<int>
struct B { static int const b; };

int f() { return B<0>::b; }

template<int I>
int const B<I>::b=A::a();
