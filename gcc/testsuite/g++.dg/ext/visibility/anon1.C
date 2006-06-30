// PR c++/21581
// Test for anonymous namespace internal linkage

// { dg-do compile }
// { dg-final { scan-assembler-not "globl.*_ZN.*1fEv" } }

namespace
{
  int f() { }
}
