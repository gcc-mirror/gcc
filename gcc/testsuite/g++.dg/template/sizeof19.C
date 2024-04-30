// PR c++/114888

template<class>
struct A {
  struct B {} *b;
  static const int c = sizeof (b) / sizeof (b[0]);
};
const int d = A<int>::c;
