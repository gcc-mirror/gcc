// PR c++/49932
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0 -Wabi=2" }

template < typename T >
auto
f1( T x )			// { dg-warning "mangle" }
  -> typename decltype( x )::type {}

template < typename T >
typename decltype( T() )::type
f2( T x ) {} // ICE on here

struct S { typedef void type; };

void g()
{
  f1( S() );
  f2( S() );
}

// { dg-final { scan-assembler "\n_?_Z2f1I1SENDtfp_E4typeET_\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z2f2I1SENDTcvT__EE4typeES1_\[: \t\n\]" } }
