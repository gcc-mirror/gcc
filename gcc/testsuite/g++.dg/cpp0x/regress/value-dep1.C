// PR c++/48265
// { dg-options -std=c++0x }

template < int > struct S
{
  S () { const int i = i; i; };
};
