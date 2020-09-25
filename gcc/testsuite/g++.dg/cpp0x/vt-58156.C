// PR c++/58156
// { dg-do compile { target c++11 } }

template <class T, class... U>
void Foo(U&...) {}

template <class T, class... U>
void Foo(const U&...) {}

void Bar() {
  const int a = 0;
  Foo<int>(a);
}
