// PR c++/58188
// { dg-do compile { target c++11 } }

struct B {};
struct A
{
  A( B );
};

struct Bar
{
  template< unsigned v >
  struct Foo
  {
    A z = B();
    unsigned value;
    Foo(): value( v ) {}
  };

  struct Baz
  {
    Foo< 8 > foo1;
    Foo< 1 > foo3;
  };
};

Bar::Baz baz;
