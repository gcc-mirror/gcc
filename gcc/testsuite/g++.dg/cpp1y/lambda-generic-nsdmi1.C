// PR c++/84520
// { dg-do compile { target c++14 } }

struct A
{
  static void foo(int);
  void (*f)(int) = [](auto i) { foo(i); };
};
