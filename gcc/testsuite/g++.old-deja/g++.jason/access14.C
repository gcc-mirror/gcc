// { dg-do assemble  }

template <class T> struct A { T t; };

template <class T> class B: private T {
 public:
  T::t; // { dg-warning "deprecated" }
};

template class B<A<int> >;
