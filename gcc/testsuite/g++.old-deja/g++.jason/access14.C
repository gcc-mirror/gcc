// { dg-do assemble  }

template <class T> struct A { T t; };

template <class T> class B: private T {
 public:
  T::t;			   // { dg-bogus "" } doesn't recognize access decl
};

template class B<A<int> >;
