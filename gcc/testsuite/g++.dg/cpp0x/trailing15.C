// PR c++/101051
// { dg-do compile { target c++11 } }

template <class T>
class Foo
{
    constexpr operator T() -> T {} // { dg-error "trailing return" }
};

struct S {
  operator int() const -> double; // { dg-error "trailing return" }
};

class A { operator auto*() -> int; }; // { dg-error "auto|trailing return" }
