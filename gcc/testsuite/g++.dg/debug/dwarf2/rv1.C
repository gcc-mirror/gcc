// { dg-do compile }
// { dg-options "-g -dA -gdwarf-4 -std=c++0x" }
// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_reference_type" 1 } }
// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_rvalue_reference_type" 1 } }

struct A { A (); ~A (); };
struct B { B (); ~B (); };

void
foo ()
{
  A v;
  A &a = v;
  B &&b = B ();
}
