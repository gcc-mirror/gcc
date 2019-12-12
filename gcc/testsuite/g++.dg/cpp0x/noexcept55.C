// PR c++/91581 - ICE in exception-specification of defaulted ctor.
// { dg-do compile { target c++11 } }

struct A {
    A() noexcept(sizeof(A)) = default;
};

A a;
