// { dg-do assemble  }

struct outer {
  template <class T> struct inner;
} o;
template <class T> struct outer::inner {};
