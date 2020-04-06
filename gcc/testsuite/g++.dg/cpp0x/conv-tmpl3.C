// PR c++/91465 - ICE with template codes in check_narrowing.
// { dg-do compile { target c++11 } }

enum class D { X };
enum class S { Z };

D foo(S) { return D{}; }
D foo(double) { return D{}; }

template <typename>
struct Bar {
  D baz(S s)
  {
    return D{foo(s)};
  }
};
