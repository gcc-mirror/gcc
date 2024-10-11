// PR c++/109918: Non virtual overloads in derived classes that don't override
// anything shouldn't cause warnings, even at -Woverloaded-virtual=2
// { dg-additional-options -Woverloaded-virtual=2 }

struct Foo
{
  virtual void g() {}
};

struct Bar : Foo
{
  virtual void g() {}
  void g(int) {}
};
