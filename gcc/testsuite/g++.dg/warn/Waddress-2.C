// PR c++/64877
// { dg-options "-Waddress" }

template<class Derived>
struct S
{
  void m() {
  }

  S()
  {
    if (&S<Derived>::Unwrap != &Derived::Unwrap)
      m();
  }

  void Unwrap() {
  }
};

struct T : public S<T>
{
};

T t;
