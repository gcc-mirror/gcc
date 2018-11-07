// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool Eq() { return requires(T t) { t == t; }; }

template<Eq T> struct Foo { };

template<typename T>
  struct S { // { dg-error "constraint failure" }
    template<Eq U> friend class Bar;

    friend class Foo<T>;
  };

struct X { };

int main() {
  S<int> si; // OK
  S<X> sx;
}
