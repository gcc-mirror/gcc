// { dg-do run  }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Jun 2001 <nathan@codesourcery.com>

// Bug 3061. Constructor vtables were wrong.

class A_base {
  public:
  virtual void foo() { }
};
class A_skel : virtual public A_base { };

class B_base : virtual public A_base { };
class B_skel : virtual public B_base, virtual public A_skel { };

class C_base : virtual public B_base { };
class C_skel : virtual public C_base, virtual public B_skel { };

class D_base : virtual public C_base { };
class D_skel : virtual public D_base, virtual public C_skel { };

class D_impl : virtual public D_skel { };

int main()
{
  D_impl i;
}
