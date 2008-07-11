// { dg-do assemble  }
// { dg-options "-fshow-column" }
// GROUPS passed constructors
// ctors file
// Subject: bug in handling static const object of the enclosing class
// Date: Tue, 1 Sep 92 10:38:44 EDT

class X
{ // { dg-error "1: note:                 X::X\\(const X&\\)" } implicit constructor
  private:
    int x;
  public:
    static const X x0;
    X( int );
};

class Y // { dg-error "1: error: new types may not be defined in a return type|1: note: \\(perhaps a semicolon is missing after the definition of 'Y'\\)" }
{
  private:
    X xx;
  public:
    Y();
}
X::X( int xi ) // { dg-error "14: error: return type specification for constructor invalid|14: note: candidates are: X::X\\(int\\)" }
{
    x = xi;
}

const X X::x0( 0 );

Y::Y() // { dg-error "6: error: no matching function for call to 'X::X\\(\\)'" }
{
    xx = X::x0;
}
