// PR c++/53166
// { dg-do compile { target c++11 } }
// { dg-options "-Waddress" }

template <typename X, X a>
struct A
{
  static_assert (a != nullptr, "oops");
  static_assert (nullptr != a, "oops");

  int f()
  {
    static_assert (a != nullptr, "oops");
    static_assert (nullptr != a, "oops");
    return 1;
  }
};

int i1;
A<int*, &i1> a1;
int i2 = a1.f();
