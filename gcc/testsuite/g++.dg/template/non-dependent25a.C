// PR c++/109480
// A version of non-dependent25.C where b's initializer is a constant
// expression.
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  void f() {
    constexpr A<int> a;
    const bool b = a.g(); // { dg-bogus "private" }
  }

private:
  constexpr bool g() const { return true; }
};

template struct A<int>;
