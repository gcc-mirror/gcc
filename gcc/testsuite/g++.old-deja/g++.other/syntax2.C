// { dg-do assemble  }
// { dg-options "-fsyntax-only" }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct B 
{
  double d;
  
  virtual void f ();
};

struct C: virtual public B
{
  char c;
};

struct D: virtual public B
{
  int i;
  virtual void f ();
};

struct E : public C, public D 
{
};

E e;

