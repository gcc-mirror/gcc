// PR c++/71576
// { dg-do compile { target c++11 } }

template < typename T > T && foo ();

struct A 
{
  int i:5;
};

void foo ()
{
  int &&j = foo < A > ().i;
}
