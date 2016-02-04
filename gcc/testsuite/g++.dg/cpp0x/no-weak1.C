// PR c++/67313
// { dg-do compile { target c++11 } }
// { dg-options "-fno-weak" }

template < class ... >
void f ()
{
}

void foo ()
{
  f ();
}
