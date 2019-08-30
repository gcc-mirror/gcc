// PR c++/89512
// { dg-do compile { target c++14 } }

struct A {
  template <typename T>
  static const int a = 0;
};

struct B {
  template <typename T>
  static int foo ()
  {
    return T::a;		// { dg-error "missing template arguments" }
  }
};

int bar ()
{
  return B::foo<A> ();		// { dg-message "required from here" }
}
