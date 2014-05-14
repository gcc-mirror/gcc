// { dg-do assemble  }
// { dg-options "-fshow-column" }
// GROUPS passed constructors
// ctors file
// Subject: bug in handling static const object of the enclosing class
// Date: Tue, 1 Sep 92 10:38:44 EDT

class X	      // { dg-message "7:X::X|candidate expects" } implicit constructor
{
  private:
    int x;
  public:
    static const X x0;
    X( int );
};

class Y // { dg-error "1:new types may not be defined in a return type" "err" }
        // { dg-message "1:\\(perhaps a semicolon is missing after the definition of 'Y'\\)" "note" { target *-*-* } 17 }
{
  private:
    X xx;
  public:
    Y();
}
X::X( int xi ) // { dg-error "14:return type specification for constructor invalid" "err" }
// { dg-message "1:X::X|candidate expects" "match candidate text" { target *-*-* } 25 }
{
    x = xi;
}

const X X::x0( 0 );

Y::Y() // { dg-error "6:no matching function for call to 'X::X\\(\\)'" }
{
    xx = X::x0;
}
