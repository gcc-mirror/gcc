// { dg-do assemble  }
// Bug: we get an error trying to build up our ideal candidate.

class C {
 public:
	C( const char * );
};

	extern	const	char	c1[];

void f2( const char * );

void f1() {
	C *fntp = new C(c1);			// Line 10
	f2( c1 );
}
