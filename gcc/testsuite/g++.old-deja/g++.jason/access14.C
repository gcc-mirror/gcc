// Build don't link:

template <class T> struct A { T t; };

template <class T> class B: private T {
 public:
  T::t;			   // gets bogus error - doesn't recognize access decl
};

template class B<A<int> >;
