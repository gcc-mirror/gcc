// PR c++/58549
// { dg-do compile { target c++1y } }
// { dg-options "" }

void foo(auto)
{
  void bar();
}
