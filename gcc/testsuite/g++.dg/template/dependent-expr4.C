// { dg-do compile }
// Origin: jbrandmeyer at users dot sourceforge dot net
// PR c++/12573: COMPONENT_REFs must be inspected for dependness.

template <bool> struct S;

template <typename K> struct Y {
  int x;
};

template <class T> struct Z {
  S< (bool)(__offsetof__(&static_cast<Y<T>*>(0)->x) == 0) >
    s;
};
