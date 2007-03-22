// { dg-do run  }
//
// egcs-2.90.06
// cannot declare friend of enclosing class using its scope, works fine
// without scope or for definition of foo::bar::f
//

class foo
{
public:
  static int f();
  class bar {
    friend int foo::f();
//  friend int f();
    static int x;
  public:
    static int f() {return foo::f();}
   };
};

int foo::bar::x;

int foo::f() {
  return bar::x;
}

int
main ()
{
  return foo::bar::f ();
}
