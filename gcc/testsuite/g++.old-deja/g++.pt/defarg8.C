// { dg-do assemble  }

// Default arguments containing more than one non-nested explicit
// template argument leads to parse error

template <class T> class foo1;
template <class T, class U> class foo2;

struct bar {
  template <class T, class U>
  bar(int i = foo1<T>::baz, // { dg-bogus "" "" { xfail *-*-* } }  - 
      int j = int(foo2<T, U>::baz), // ok
      int k = foo2<T, U>::baz) {} // { dg-bogus "" "" { xfail *-*-* } }  - before > - 
};
