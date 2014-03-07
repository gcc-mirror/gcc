// PR c++/49003
// { dg-do compile { target c++11 } }

struct A {
    auto a() const -> decltype(this) { return this; }
};
