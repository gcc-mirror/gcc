// PR c++/49003
// { dg-options -std=c++0x }

struct A {
    auto a() const -> decltype(this) { return this; }
};
