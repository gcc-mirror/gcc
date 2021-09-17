// PR c++/101904
// Verify we stop at the first bad argument conversion when considering a
// candidate during overload resolution.

template<class T>
struct A { typedef typename T::type type; };

struct B {
  // A conversion function that always induces a hard error when instantiated.
  template<class T> B(T, typename A<T>::type = 0);
};

struct C {
  template<class T> void f(T, typename A<T>::type); // #1
  template<class T> void f(T, T) const;             // #2

  static void g(int*, B);                           // #3
  static void g(int, int);                          // #4

#if __cpp_ref_qualifiers
  void h(B) &;                                      // #5
  void h(int) &&;                                   // #6
#endif
};

int main() {
  const C c;

  // The bad conversion for the 'this' argument should preclude us from further
  // considering the non-const #1 (which would have caused a hard error during
  // instantiation).  This behavior is essentially DR 1391 extended to the
  // 'this' argument.
  c.f(0, 0); // resolves to #2
  c.f<int>(0, 0);

  // Likewise for the bad conversion for the 1st argument in #3.
  C::g(42, 42); // resolves to #4

#if __cpp_ref_qualifiers
  // Likewise for the bad 'this' conversion in #5.
  C().h(0); // resolves to #6
#endif
}

#if __cpp_concepts
// Test the same calls in a SFINAE context.
template<class T>
concept D = requires (const T t) {
  t.f(0, 0);
  t.template f<int>(0, 0);
  T::g(42, 42);
  T().h(0);
};

static_assert(D<C>);

// Test that when there's no strictly viable candidate and we're in a
// SFINAE context, we still stop at the first bad argument conversion.
template<class T>
concept E = requires { T().h(nullptr); };

static_assert(!E<C>);
#endif
