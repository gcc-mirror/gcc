// { dg-do assemble  }

template <class T>
struct S1 {};

template <class T>
void f(T);

template <class C>
struct S2
{
  template <class T>
  void f<S1<T> >(T) {}  // { dg-error "" } bad specialization.
};


template <class T>
struct S3
{
  friend class S2<T>;
};
