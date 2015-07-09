// { dg-do compile { target c++11 } }
// { dg-options "-gdwarf-2 -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_enumeration_type" 3 } }
// { dg-final { scan-assembler-times " DW_AT_enum_class" 2 } }

enum A { a1, a2 } a;
enum struct B { b1, b2 } b;
enum class C { c1, c2 } c;

void
foo ()
{
  a = a1;
  a = A::a2;
  b = B::b1;
  b = B::b2;
  c = C::c1;
  c = C::c2;
}
