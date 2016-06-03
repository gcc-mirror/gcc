// PR c++/27100
// This used to fail at link time with an "undefined reference to 'foo'" error.
// { dg-do run }

struct A
{
  friend void foo (const A&) { }
  friend void foo (const A&);
};

int
main ()
{
  foo (A ());
}
