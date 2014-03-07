// PR c++/42370
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

void foo()
{
  []{ return 0; }();
} // { dg-bogus "no return statement" }
