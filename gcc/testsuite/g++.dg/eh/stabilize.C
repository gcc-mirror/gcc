// PR c++/8186

// Bug: In f, convert_for_arg_passing wrapped the A TARGET_EXPR in an
// ADDR_EXPR for passing by invisible ref.  stabilize_throw_expr copied the
// resulting pointer into a temporary.  cp_convert_parm_for_inlining then
// dereferences it and tries to initialize B::am with the INDIRECT_REF,
// which calls for a bitwise copy.  Which is broken.

// { dg-options "-O" }

struct A
{
  A();
  A(const A&);
  A& operator=(const A&);
};

struct B {
  A am;
  B(A a) { am = a; }
};

void f ()
{
  throw B(A());
}
