// Test for propagation of anonymous namespace internal linkage

// { dg-do compile }
// { dg-final { scan-assembler-not "globl.*_Z1fv" } }
// { dg-require-visibility "" }

namespace
{
  struct A { };
}

A f () { }
