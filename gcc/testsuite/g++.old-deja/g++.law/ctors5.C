// Build don't link: 
// GROUPS passed constructors
// ctors file
// Subject: bug in handling static const object of the enclosing class
// Date: Tue, 1 Sep 92 10:38:44 EDT

class X
{
  private:
    int x;
  public:
    static const X x0;
    X( int );
}; // ERROR - candidate

class Y
{
  private:
    X xx;
  public:
    Y();
}
X::X( int xi )
{// ERROR -  return.*
    x = xi;
}

const X X::x0( 0 );

Y::Y()
{// ERROR -  no mat
    xx = X::x0;
}
