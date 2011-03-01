// PR c++/46282
// { dg-options -std=c++0x }

template<int>
class A
{
  A : i() {}			// { dg-message "" }
  int i;
};
