// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Oct 2000 <nathan@codesourcery.com>
// Origin: bug 511 malte.starostik@t-online.de

// Bug 511. We failed to set access correctly for enumeration members of
// template classes

template <class>
class A
{
  public:
  enum foo {bar};
  typedef int baz;
};

struct C: public A<int>
{
  void choke (foo);
  void choke (baz);
};

template <class>
struct B
{
  private:
  enum foo {bar}; // { dg-error "" } private
  typedef int baz;  // { dg-error "" } private
};

struct D: public B<int>
{
  void choke (foo);   // { dg-error "" } within this context
  void choke (baz);   // { dg-error "" } within this context
};

