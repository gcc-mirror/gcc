// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/40109
// { dg-do compile }
// { dg-options  "-gdwarf-2 -dA -O0" }

namespace A
{

  class B
  {
  };
  typedef A::B AB;
};

int
main()
{
  A::AB ab;
  return 0;
}

// { dg-final { scan-assembler "DW_TAG_typedef" } }
//
// What we want to do here is to be sure that the DIE of A::AB is generated
// as a child of the DIE of the namespace A declaration.
// So this test won't catch a regression on this fix yet. To write a proper
// test for this fix, we would need a dwarf reader written in tcl,
// or something along those lines.

