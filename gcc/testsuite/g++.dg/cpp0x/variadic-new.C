// { dg-do run { target c++11 } }
// Contributed by Peter Dimov
// PR c++/32597
#include <assert.h>
#include <new>

int k = 5;

template< class... Args > void f( Args... args )
{
   new( &k ) int( args... );
}

int main()
{
   f();
   assert( k == 0 );
}
