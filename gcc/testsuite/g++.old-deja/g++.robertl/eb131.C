// Build don't link
// The call to a::foo() generates an error:
// eb131.C: In method `a::a()':
// eb131.C:26: no matching function for call to `a::foo (void (a::*)(double))'
// eb131.C:15: candidates are: a::foo(void (a::*)(float))
// According to [over.over] in the CD2, &junk should resolve in this context.
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/18.

struct a {
       a();
       void junk();
       void junk( int );
       void junk( float );
       void junk( double );

       void foo( void (a::*member)(float) );
       void bar( void (*function)(float) );
};

void baz();
void baz( int );
void baz( float );
void baz( double );

a::a()
{
       foo( &junk );
       bar( &baz );
}
