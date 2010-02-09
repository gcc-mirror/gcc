// PR c++/42370
// { dg-options "-std=c++0x -Wall" }

void foo()
{
  []{ return 0; }();
} // { dg-bogus "no return statement" }
