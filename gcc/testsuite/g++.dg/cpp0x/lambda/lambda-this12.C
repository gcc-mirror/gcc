// Uses of 'this' in unevaluated context are not odr-uses.
// { dg-do compile { target c++11 } }

struct A
{
  int f() {}
  int i;

  void foo()
  {
    [] () { sizeof(i); sizeof(f()); };
  }
};
