// { dg-do assemble  }
// GROUPS passed member-pointers error-reporting
struct Y
{
  struct X
    {
      int A;
      int Y::X::* foo () { undef1(1); return &Y::X::A; }// { dg-error "28:'undef1' was not declared" } foo().*
      int bar () { return A; }
    };
};

int Y::X::* foo ()
{
  undef2(1);// { dg-error "3:'undef2' was not declared" } foo().*
  return &Y::X::A;
}

int Y::X::* (* foo2 ())()
{
  undef3(1);// { dg-error "3:'undef3' was not declared" } foo().*
  return foo;
}

int (Y::X::* bar2 ()) ()
{
  undef4(1);// { dg-error "3:'undef4' was not declared" } foo\(\).*
  return Y::X::bar;// { dg-error "" } foo\(\).*
}

int Y::X::* (Y::X::* foo3 ())()
{
  undef5(1);// { dg-error "3:'undef5' was not declared" } foo().*
  return Y::X::foo;// { dg-error "" } foo().*
}
