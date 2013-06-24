// PR c++/16128

template<typename T>
struct A {
    };

namespace H {
    template<typename T>
    struct B {};
    }

A a;             // { dg-error "template" }
H::B b;          // { dg-error "template" }

int main() {
    A a;         // { dg-error "template" }
    H::B b;      // { dg-error "template" }
    return 0;
    }
