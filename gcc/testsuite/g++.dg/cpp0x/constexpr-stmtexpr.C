// PR c++/46977
// { dg-options "-std=c++0x" }

template < typename > void
foo ()
{
  ({int i;}), 0;
}
