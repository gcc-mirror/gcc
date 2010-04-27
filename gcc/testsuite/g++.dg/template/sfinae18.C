// PR c++/41468

typedef int Ft(int);
struct A { operator Ft*(); };
struct B { operator Ft*(); };
struct C : A, B { };

template<typename C> void f(int (*a)[sizeof(C()(0))]);
template<typename C> void f(...);
int main() { f<C>(0); }
