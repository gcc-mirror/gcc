// PR c++/78690

struct C;

template <typename T>
struct A
{
  struct C { static void bar (); };
};

template <typename T>
struct B
{
  using A<T>::C;
  void
  foo () { C.bar (); }
};
