// Test that the A* pointer_type is also within the debug info for f.
// Currently GCC emits it immediately before A, which is simple to test for.
// { dg-options "-gdwarf-2 -dA" }

void f()
{
  int j = 5;
  {
    struct A { int i; } *ap;
    ap->i = 42;
  }
}

// { dg-final { scan-assembler "DW_TAG_pointer_type.\[^)\]*. DW_TAG_structure_type" } }
