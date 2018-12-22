// PR c++/58678
// { dg-do compile }
// { dg-options "-O3 -fdump-ipa-devirt" }

// We shouldn't speculatively devirtualize to ~B because B is an abstract
// class; any actual object passed to f will be of some derived class which
// has its own destructor.

struct A
{
  virtual void f() = 0;
  virtual ~A();
};

struct B : A
{
  virtual ~B() {}
};

void f(B* b)
{
  delete b;
}

// { dg-final { scan-ipa-dump-not "Speculatively devirtualizing" "devirt" } }
