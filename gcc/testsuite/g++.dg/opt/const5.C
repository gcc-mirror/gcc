// We don't have a good way of determining how ".rodata" is spelled on
// all targets, so we limit this test to a few common targets where we
// do know the spelling.
// { dg-do compile { target i?86-*-linux* x86_64-*-linux* } }
// { dg-final { scan-assembler "\\.rodata" } }

template <typename T>
struct B {
  int i;
};

// This declaration should be placed in .rodata.
const B<int> const_B __attribute__((used)) = { 3 };
