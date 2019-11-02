// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Test implicit conversion of pointers to member functions appearing as
// operands of the equality operators.

struct B { };

struct BV { };

struct D : B, virtual BV { };

struct C { };

void f ()
{
  void (D::*pd) () = 0;
  void (B::*pb) () = 0;
  void (BV::*pbv) () = 0;
  void (C::*pc) () = 0;

  pd == pb;
  pd == pbv;  // { dg-error "" }
  pd == pc;   // { dg-error "6:comparison between distinct pointer-to-member types" }
}
