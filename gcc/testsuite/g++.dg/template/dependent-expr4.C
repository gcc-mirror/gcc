// { dg-do compile }
// Origin: jbrandmeyer at users dot sourceforge dot net
// PR c++/12573: COMPONENT_REFs must be inspected for dependness.
// Or, more specifically OFFSETOF.

template <bool> struct S;

template <typename K> struct Y {
  int x;
};

template <class T> struct Z {
  S< (bool)(__builtin_offsetof (Y<T>*, x) == 0) >
    s;
};
