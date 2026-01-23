// PR c++/123663
// { dg-do compile { target c++20 } }

template <typename T>
concept TestConcept = requires {
  new T[1]{};
};
struct base {
  base() {
    struct Foo {};
    TestConcept<Foo>;
  }
};
