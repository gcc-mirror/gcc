// PR c++/59818

template <class T>
struct Identity {
  typedef T type;
};

struct Foo {
  template <typename T>
  Foo(T*, void (Identity<T>::type::*m)(void));
};

struct Bar {
  void Method(void) const;
};

void Bar::Method(void) const
{
  Foo foo(this, &Bar::Method);	// { dg-error "no match" }
}
