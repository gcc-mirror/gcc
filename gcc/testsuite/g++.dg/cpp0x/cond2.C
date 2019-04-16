// PR c++/86205
// { dg-do compile { target c++11 } }

bool b;

template < class T > int f ()
{
  return 0;
}

template < class T > auto g () -> decltype (b ? f < int > : throw 0)
{
  return b ? f<int> : throw 0;
}
