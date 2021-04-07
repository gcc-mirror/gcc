// PR c++/95066 - explicit malfunction with dependent expression.
// { dg-do compile { target c++20 } }

template <typename T>
struct Foo {
  template <typename U>
  explicit(static_cast<U>(true)) operator Foo<U>();
};

template <typename T>
template <typename U>
Foo<T>::operator Foo<U>() {
  return {};
}

int
main ()
{
  Foo<float> a;
  Foo<int> b = a; // { dg-error "conversion" }
}
