// Test that we can use mixins with COM classes.

struct A
{
  virtual int foo () = 0;
  virtual int bar () = 0;
} __attribute__((__com_interface__));

struct B
{
  virtual int baz () { return 5; }
};

struct C : public A, public B
{
  int foo () { return 0; }
  int bar () { return 1; }
};

int main ()
{
  C c;
  return c.foo ();
}          
