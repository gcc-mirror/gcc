// PR c++/48265

template < int > struct S
{
  S () { const int i = i; i; };
};
