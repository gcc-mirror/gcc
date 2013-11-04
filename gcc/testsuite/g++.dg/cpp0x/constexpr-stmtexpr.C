// PR c++/46977
// { dg-options "-std=c++11" }

template < typename > void
foo ()
{
  ({int i;}), 0;
}
