// PR c++/58549
// { dg-do compile { target c++14 } }
// { dg-options "" }

void foo(auto)
{
  void bar();
}
