// PR c++/16128

template<typename T>
struct A {
    };

namespace H {
    template<typename T>
    struct B {};
    }

A a;             // { dg-error "template|no match" }
H::B b;          // { dg-error "template|no match" }

int main() {
    A a;         // { dg-error "template|no match" }
    H::B b;      // { dg-error "template|no match" }
    return 0;
    }
