// PR c++/47125

template < bool, typename >
struct enable_if {};

template < typename T >
struct enable_if< true, T >
{
    typedef T type;
};

template < typename T >
struct enable_if< true, T >::type
f( T x );			// { dg-error "not a class type" }

void
g( void )
{
  f< int >( 0 );		// { dg-error "no match" }
}

// { dg-prune-output "note" }
