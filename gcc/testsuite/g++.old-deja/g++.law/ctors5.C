// { dg-do assemble  }
// GROUPS passed constructors
// ctors file
// Subject: bug in handling static const object of the enclosing class
// Date: Tue, 1 Sep 92 10:38:44 EDT

class X
{ // { dg-error "" } candidate
  private:
    int x;
  public:
    static const X x0;
    X( int );
};

class Y
{
  private:
    X xx;
  public:
    Y();
}
X::X( int xi )
{// { dg-error "" }  return.*
    x = xi;
}

const X X::x0( 0 );

Y::Y()
{// { dg-error "" }  no mat
    xx = X::x0;
}
