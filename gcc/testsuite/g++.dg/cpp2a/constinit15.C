// PR c++/92058 - constinit malfunction in static data member.
// { dg-do compile { target c++20 } }

struct B {
    B() {}
};

struct A {
    constinit static inline B b1{}; // { dg-error "does not have a constant initializer|call to non-.constexpr. function" }
};

int main() {
    A a;
}
