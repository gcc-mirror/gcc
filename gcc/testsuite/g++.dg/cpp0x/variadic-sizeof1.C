// PR c++/56679
// { dg-require-effective-target c++11 }

template <template <typename> class... Args>
struct Foo {
  static const int value = sizeof...(Args);
};

template <typename> struct Bar { };

const int test = Foo<Bar>::value;
