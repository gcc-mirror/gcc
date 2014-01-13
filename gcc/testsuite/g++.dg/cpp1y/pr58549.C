// PR c++/58549
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

void foo(auto)
{
  void bar();
}
