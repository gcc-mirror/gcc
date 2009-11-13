// PR debug/26965
// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler-not "DW_TAG_variable" } }
// { dg-final { scan-assembler-not "DW_TAG_enumerator" } }
// { dg-final { scan-assembler-not "DW_TAG_enumeration_type" } }

enum x { i = 1 };
class c {
  static const x beg = i;
  int foo () { return (int) beg; }
};
void bar () { }
