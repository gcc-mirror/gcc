// { dg-do assemble  }

template <class T>
struct S1 {};

template <class T>
void f(T);

template <class C>
struct S2
{
  template <class T>
  void f<S1<T> >(T) {}  // { dg-error "8:non-class, non-variable partial specialization" "" { target c++14 } }
  // { dg-error "8:non-type partial specialization" "" { target c++11_down } .-1 }
};


template <class T>
struct S3
{
  friend class S2<T>;
};
