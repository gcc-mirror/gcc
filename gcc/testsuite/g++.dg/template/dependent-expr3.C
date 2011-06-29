// { dg-options -std=c++98 }
// { dg-do compile }
// Origin: jbrandmeyer at users dot sourceforge dot net
// PR c++/12573: COMPONENT_REFs must be inspected for dependness.

template <bool> struct S;

template <typename K> struct Y : K {
  int x;
};

template <class T> struct Z {
  S< (bool)(&static_cast<Y<T> *>(0)->x == 0) > // { dg-error "" }
  s;
};
