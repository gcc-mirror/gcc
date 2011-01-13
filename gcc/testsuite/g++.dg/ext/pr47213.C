// { dg-do compile }
// { dg-options "-fvisibility-ms-compat" }
#include <typeinfo>

template < typename T > void
bar ()
{
  typeid (T);
}

void
foo ()
{
  bar < int () > ();
}
