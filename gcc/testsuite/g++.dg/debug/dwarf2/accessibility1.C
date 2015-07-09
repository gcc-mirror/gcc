// PR debug/44668
// { dg-do compile }
// { dg-options "-gdwarf-2 -dA" }

struct C
{
private:
  typedef int a;
  a b;
  enum g { g1, g2 } h;
  struct D { int i; } i;
protected:
  typedef int c;
  c d;
public:
  typedef int e;
  e f;
} c;

// 3 private DW_TAG_member dies, 1 private DW_TAG_typedef,
// 1 private DW_TAG_enumeration_type and 1 private DW_TAG_structure_type
// { dg-final { scan-assembler-times "3\[^\\r\\n\]* DW_AT_accessibility" 6 } }
// 1 private DW_TAG_member die, 1 private DW_TAG_typedef
// { dg-final { scan-assembler-times "2\[^\\r\\n\]* DW_AT_accessibility" 2 } }
