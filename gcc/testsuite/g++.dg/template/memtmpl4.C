// PR c++/53492

template<typename T> struct A
{
  template<typename U> struct B;
};

template <> template<class T> struct A<T>::B { }; // { dg-error "expected 2 levels" }

A<int>::B<int> b;		// { dg-error "" }
