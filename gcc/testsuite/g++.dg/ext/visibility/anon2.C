// Test for propagation of anonymous namespace internal linkage

// { dg-do compile }
// { dg-final { scan-assembler-not "globl.*_Z1fv" { xfail *-*-aix* } } }
// { dg-require-visibility "" }

namespace
{
  struct A { };
}

A f () { return A(); }
