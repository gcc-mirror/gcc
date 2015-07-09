// Origin: PR debug/43628
// { dg-options "-gdwarf-2 -dA -fno-debug-types-section" }
// { dg-do compile }

// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_formal_parameter\\)\[\n\r\]{1,2}\[^\n\r\]*DW_AT_type\[\n\r\]{1,2}" 1 } }
class C
{
  public:
  typedef void (*t) (C);
};
C::t f;
