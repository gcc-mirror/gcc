// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58549

void foo(auto)
{
  void bar();
}

