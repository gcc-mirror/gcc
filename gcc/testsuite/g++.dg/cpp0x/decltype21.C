// PR c++/6709 (DR 743)
// PR c++/42603 (DR 950)
// { dg-options -std=c++11 }

template <class T>
T make();

struct p { typedef int t; };
struct c : decltype(make<p>()) {};

decltype(make<p>())::t t;

// PR c++/49823

template < typename T >
auto f( const T &x )
  -> typename decltype( x )::type; // ICE on here

template < typename T >
typename decltype( T{} )::type // ICE on here
f( T );

template < typename T >
void f( T x )
{ typename decltype( x )::type t; } // ICE on here

// Negative tests

int f();
decltype(f())::t t2;		// { dg-error "not a class" }

struct D: decltype(f()) { };	// { dg-error "not a class" }

// { dg-prune-output "expected initializer" }
