// PR c++/41437
// { dg-do compile }

class A { struct B { B(); }; };
template<typename T> void f() { A::B b; } // { dg-error "private" }
void g() { f<int>(); }

class X { template<typename> struct A{}; };

X::A<int> a; // { dg-error "private" }
