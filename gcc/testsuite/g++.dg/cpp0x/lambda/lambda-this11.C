// PR c++/51494, c++/56222
// Uses of static members and creating pointers to members aren't odr-uses
// of 'this'.
// { dg-do compile { target c++11 } }

struct A
{
  static void f() {}
  static int i;
  int j;
  void f(int);

  void foo()
  {
    [] () {
      ++i;
      f();
      &A::j;
      (void(*)())&A::f;
    };
  }
};
