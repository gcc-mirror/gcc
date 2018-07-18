// { dg-options "-O2 -gdwarf-4 -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times " DW_AT_export_symbols" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\r\]*\\) DW_TAG_imported_module" 2 } }

namespace A
{
  int i = 5;
  inline namespace B
  {
    int j = 6;
    namespace C
    {
      int k = 7;
    };
  };
};
int l = A::i + A::j + A::C::k;
int m = A::i + A::B::j + A::B::C::k;
namespace
{
  int n = 8;
};
int o = n;
