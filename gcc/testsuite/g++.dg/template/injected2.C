// DR 1004

template <class T, template<class>class U = T::template B> struct A { };

template <class T> struct B {
  template <class U> friend struct B;
};

A<B<int> > a;
