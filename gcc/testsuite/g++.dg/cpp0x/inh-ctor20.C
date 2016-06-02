// PR c++/70972
// { dg-do compile { target c++11 } }

struct moveonly {
    moveonly(moveonly&&) = default;
    moveonly() = default;
};

struct A {
    A(moveonly) {}
};
struct B : A {
    using A::A;
};

B b(moveonly{});
