// Origin PR c++/51143
// { dg-do compile { target c++11 } }

using A0 = struct B0 { void f() {} };

template<int N>
using A1 =
    struct B1 { // { dg-error "types may not be defined in alias template" }
        static auto constexpr value = N;
    };

A1<0> a1;

template<class T>
using A2 =
    struct B2 {  // { dg-error "types may not be defined in alias template" }
        void f(T){}
    };

A2<bool> a2;

template<class T>
using A3 =
    enum B3 {b = 0;}; //{ dg-error "types may not be defined in alias template" }

A3<int> a3; // { dg-error "'A3' does not name a type" }

int main() { }
