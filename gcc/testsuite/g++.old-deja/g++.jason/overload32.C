// PRMS Id: 7128
// Build don't link:

class B {};

class D : public B {};

class X {
 public:
   operator const B & () const;
   operator const D & () const;
};
	
void f( const D & );

void g( const X & x )
{
   f( x );
};
