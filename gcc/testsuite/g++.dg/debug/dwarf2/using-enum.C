// Test of 'using enum' debug info.
// { dg-do compile { target c++20 } }
// { dg-options "-g -dA" }

struct A
{
  // All the counts are +1 for the abbreviation table.
  // { dg-final { scan-assembler-times "DW_TAG_enumeration_type" 2 } }
  // { dg-final { scan-assembler-times "DW_TAG_enumerator" 3 } }
  enum E { e, f };
};

struct B
{
  // The using-enum-declaration is represented by two
  // DW_TAG_imported_declaration, one for each enumerator.
  // { dg-final { scan-assembler-times "DW_TAG_imported_declaration" 3 } }
  using enum A::E;
};

B b;
