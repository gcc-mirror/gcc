// PR c++/97663

template <class T> struct foo {};
template <class T> struct bar {};
template <class T> struct baz {};
template <class T> struct qux {};
template <class T> struct corge {};

namespace N {
  unsigned foo ();
  signed bar ();
  long baz ();
  long long qux ();
  short corge (); 
}
