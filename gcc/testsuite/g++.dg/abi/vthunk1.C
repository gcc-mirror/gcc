// { dg-do link }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Jul 2001 <nathan@codesourcery.com>

// Origin snyder@fnal.gov
// Bug 3631. We mis-calculated the non-virtual part of a virtual
// thunk. Leading to a link failure, in this case.

struct A { virtual ~A () {} };

struct B : virtual public A
{
  virtual void destroy() {}
};

class C : virtual public B {};
class D : virtual public C {};
class E : public virtual A {};

struct F : virtual public B, virtual public E
{
  virtual void destroy() = 0;
};

struct G : public virtual F
{
  virtual void destroy() {}
};

class H : virtual public C, virtual public F {};
class I : virtual public D, virtual public H {};
class J : public virtual G, public virtual H {};

class K : public virtual I, public virtual J
{
  public:
  virtual ~K();
};
K::~K() {}

int main ()
{
  return 0;
}
