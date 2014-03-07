// PR c++/46282

template<int>
class A
{
  A : i() {}			// { dg-message "" }
  int i;
};
