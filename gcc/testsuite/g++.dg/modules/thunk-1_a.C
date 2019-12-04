// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

export struct Base
{
  virtual ~Base () {}
  int m;
};

// Force the creation of implicit members, because we get that wrong
// in the importer and think its imported (as of 2018-11-27 this
// causes an ICE, previously it happened to work in this particular
// testcase.  
Base x;

export struct Derived : virtual Base 
{
  ~Derived () {}  
};

Derived y;
