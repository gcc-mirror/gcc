// PR c++/54122
// { dg-options -std=c++11 }

enum E { F };

template <typename A>
struct C
{
  E e;
  void f () { auto l = [&](void)->void { if (e == F) return; }; }
};
