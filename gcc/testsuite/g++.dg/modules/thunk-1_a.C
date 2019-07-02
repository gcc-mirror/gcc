// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

export struct Base
{
  virtual ~Base () {}
  int m;
};

export struct Derived : virtual Base 
{
  ~Derived () {}  
};
