// PR c++/82357

template <typename> struct A {
  A() { x |= 0; }
  int x : 8;
};
