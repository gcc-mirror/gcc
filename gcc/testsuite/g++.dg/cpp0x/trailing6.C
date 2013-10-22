// PR c++/49003
// { dg-options -std=c++11 }

struct A {
    auto a() const -> decltype(this) { return this; }
};
