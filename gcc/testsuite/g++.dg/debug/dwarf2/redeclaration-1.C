// Origin: PR debug/43325
// { dg-options "-gdwarf-2 -dA" }
// { dg-do compile }

// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE \[^\n\r\]*DW_TAG_lexical_block\\)\[\n\r\]{1,2}\[^\n\r\]*DW_AT_low_pc\[\n\r\]{1,2}\[^\n\r\]*DW_AT_high_pc\[\n\r\]{1,2}\[^\n\r\]*\\(DIE \[^\n\r\]*DW_TAG_variable\\)\[\n\r\]{1,2}\[^\n\r\]*DW_AT_name" 2 } }

namespace S
{
  int
  f()
  {
    {
      int i = 42;
      {
	extern int i;
	return i;
      }
    }
  }
}
