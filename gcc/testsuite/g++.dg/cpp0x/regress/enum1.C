// PR c++/47482
// { dg-options -std=c++0x }

template<class>
struct K
{
  enum { A = sizeof"A", B = +A };
};
