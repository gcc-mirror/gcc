// Build don't link:
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/18.

struct a {
	a();
	void junk( float );
	void junk( double );

	void bar( double );
	void bar( float );

  void foo( void (a::*member)(float) ); // ERROR - candidate
};

a::a()
{
	foo( &junk ); // ERROR - junk is an unqualified-id.
	foo( &bar );  // ERROR - bar is an unqualified-id.
}
