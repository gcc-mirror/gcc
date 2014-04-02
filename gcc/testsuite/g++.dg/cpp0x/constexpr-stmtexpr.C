// PR c++/46977
// { dg-do compile { target c++11 } }
// { dg-options "" }

template < typename > void
foo ()
{
  ({int i;}), 0;
}
