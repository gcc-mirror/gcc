// { dg-do compile }
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// Origin: Steven Bosscher <steven at gcc dot gnu dot org>
// PR c++/17401: ICE with invalid pure specifier

// NOTE: This also tests QoI of diagnostic for invalid pure specifiers.
//  Please do *not* relax the dg-error tests.

class foo
{
  virtual void bar1 () = 0;
  virtual void bar2 () = __null;  // { dg-error "32:invalid pure specifier" }
  virtual void bar3 () = 4;       // { dg-error "27:invalid pure specifier" }
  virtual void bar4 () = A::f;    // { dg-error "27:invalid pure specifier" }
  virtual void bar5 () = 0l;      // { dg-error "28:invalid pure specifier" }
  virtual void bar6 () = 00;      // { dg-error "28:invalid pure specifier" }
  virtual void bar7 () = 0x0;     // { dg-error "29:invalid pure specifier" }
};
