// Build don't link:

// Default arguments containing more than one non-nested explicit
// template argument leads to parse error

template <class T> class foo1;
template <class T, class U> class foo2;

struct bar {
  template <class T, class U>
  bar(int i = foo1<T>::baz, // ok
      int j = int(foo2<T, U>::baz), // ok
      int k = foo2<T, U>::baz) {} // gets bogus error - before > - XFAIL *-*-*
};
