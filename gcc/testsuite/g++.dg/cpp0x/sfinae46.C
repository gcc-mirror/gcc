// PR c++/57419
// { dg-do compile { target c++11 } }

template< typename q >
decltype( &q::f ) t( q ) {}

char t( ... ) { return {}; }

class c { void f() = delete; };
class d { static void f() = delete; };

static_assert( sizeof( t( c() ) ), "c" );
static_assert( sizeof( t( d() ) ), "d" );
