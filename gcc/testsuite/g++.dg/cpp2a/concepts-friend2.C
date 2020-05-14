// { dg-do compile { target c++20 } }

template<typename T>
  concept Eq = requires(T t) { t == t; }; // { dg-message "in requirements" }

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
