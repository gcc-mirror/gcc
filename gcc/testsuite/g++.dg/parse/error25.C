// { dg-do compile }
// Origin: Steven Bosscher <steven at gcc dot gnu dot org>
// PR c++/17401: ICE with invalid pure specifier

// NOTE: This also tests QoI of diagnostic for invalid pure specifiers.
//  Please do *not* relax the dg-error tests.

class foo
{
  virtual void bar1 () = 0;
  virtual void bar2 () = __null;  // { dg-error "invalid pure specifier" }
  virtual void bar3 () = 4;       // { dg-error "invalid pure specifier" }
  virtual void bar4 () = A::f;    // { dg-error "invalid pure specifier" }
};


