// { dg-do assemble  }

// Default arguments containing more than one non-nested explicit
// template argument leads to parse error

// This might be ill formed. See DR 325 (which would like to make it
// so)

template <class T> class foo1;
template <class T, class U> class foo2; // { dg-error "" }

struct bar {
  template <class T, class U>
  bar(int i = foo1<T>::baz, // { dg-bogus "" }  - 
      int j = int(foo2<T, U>::baz), // ok
      int k = foo2<T, U>::baz) {} // { dg-error "" }
};
