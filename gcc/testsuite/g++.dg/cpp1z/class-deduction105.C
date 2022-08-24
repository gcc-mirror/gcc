// PR c++/82632
// { dg-do compile { target c++17 } }

template<class T, int = 1>
struct Foo {
  Foo() = default;
  Foo(const Foo&) = delete;

  template<int I>
  Foo(const Foo<T, I>&);
};

template<class T, int I>
Foo(Foo<T,I>) -> Foo<T,I+1>;

Foo<int> a;
Foo b = a;
