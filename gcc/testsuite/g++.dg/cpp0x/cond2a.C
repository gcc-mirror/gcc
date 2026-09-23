// PR c++/126406
// { dg-do compile { target c++14 } }
// A version of cond2.C where f has a deduced return type
// and g is instantiated.

bool b;

template < class T > auto f ()
{
}

template < class T > auto g () -> decltype (b ? f < int > : throw 0)
{
  return b ? f<int> : throw 0;
}

using type = decltype(g<int>());
