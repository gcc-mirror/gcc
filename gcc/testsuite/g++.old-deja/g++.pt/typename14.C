// { dg-do assemble  }
// { dg-options "-Wno-deprecated" }

template <class T>
struct B {
  typedef T X;
};

template <class T>
struct S : public B<T>
{
  struct I {
    void f(X x);   // { dg-error "'X' has not been declared" } implicit typename
  };
};
