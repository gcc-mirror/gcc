// PR c++/62255

// It's not clear whether this is well-formed; instantiating the
// initializer of 'value' causes the instantiation of Derived, which in
// turn requires the value of 'value', but the recursion ends there, so it
// seems reasonable to allow it.

template <typename T> struct Test {
  template<typename X> static int check(typename X::Type*);
  template<typename> static char check(...);
  static const bool value = (sizeof(check<T>(0)) == sizeof(int));
};
template <int> struct Sink { };
template <typename T> struct Derived : Sink<Test<Derived<T> >::value> {
  typedef int Type;
};

Sink<Test<Derived<int> >::value> s;
