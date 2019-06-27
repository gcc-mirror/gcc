// PR c++/70156
// { dg-do compile { target c++11 } }

template <int> struct A { int i; };
struct B { int i; };
struct C {
    static const A<0> a = { 0 }; // { dg-error ".constexpr. needed for in-class initialization of static data member" }
    static const B b = { 1 }; // { dg-error ".constexpr. needed for in-class initialization of static data member" }
};
