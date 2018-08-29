// { dg-options "-O2 -gdwarf-5 -dA" }
// { dg-final { scan-assembler-times " DW_AT_export_symbols" 2 } }
// { dg-final { scan-assembler-not "DIE \\(\[^\n\r\]*\\) DW_TAG_imported_module" } }

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
