// PR c++/58633
// { dg-do compile { target c++11 } }

void foo(int i)
{
  typedef int I;
  decltype(i.I::~I())* p;
}
