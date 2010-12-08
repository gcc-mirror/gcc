// { dg-do assemble  }
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/18.

struct a {
	a();
	void junk( float );
	void junk( double );

	void bar( double );
	void bar( float );

  void foo( void (a::*member)(float) );   // { dg-message "void a::foo|no known conversion" } 
};

a::a()
{
	foo( &junk ); // { dg-error "match" } junk is an unqualified-id.
	// { dg-message "candidate" "candidate note" { target *-*-* } 18 }
	foo( &bar );  // { dg-error "match" } bar is an unqualified-id.
	// { dg-message "candidate" "candidate note" { target *-*-* } 20 }
}
