// PR c++/124154
// { dg-do compile { target c++23 } }

struct S
{
  int x;
  void foo () { auto l = [&] (this auto) { (void) x; }; l (); }
};
