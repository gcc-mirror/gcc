// PR c++/14002

template <typename T> void foo (T x) { x; }

void bar() { foo(0); }

struct A
{
  friend void foo<int> (int);
};
