// PR c++/13983, c++/17519
// The typedef and the array were causing us to miss that A<int> is
// a packed type.

template <class T>
struct A {
  A();
} __attribute__((packed));

typedef A<int> Ai;

struct B {
  Ai a[2];
} __attribute__((packed));
