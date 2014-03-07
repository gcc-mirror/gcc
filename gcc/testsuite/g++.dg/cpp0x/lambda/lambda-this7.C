// PR c++/54122
// { dg-do compile { target c++11 } }

enum E { F };

template <typename A>
struct C
{
  E e;
  void f () { auto l = [&](void)->void { if (e == F) return; }; }
};
