// Build don't link: 
// GROUPS passed member-pointers error-reporting
struct Y
{
  struct X
    {
      int A;
      int Y::X::* foo () { undef1(1); return &Y::X::A; }// ERROR - foo().*
      int bar () { return A; }
    };
};

int Y::X::* foo ()
{
  undef2(1);// ERROR - foo().*
  return &Y::X::A;
}

int Y::X::* (* foo2 ())()
{
  undef3(1);// ERROR - foo().*
  return foo;
}

int (Y::X::* bar2 ()) ()
{
  undef4(1);// ERROR - foo\(\).*
  return Y::X::bar;// ERROR - foo\(\).*
}

int Y::X::* (Y::X::* foo3 ())()
{
  undef5(1);// ERROR - foo().*
  return Y::X::foo;// ERROR - foo().*
}
