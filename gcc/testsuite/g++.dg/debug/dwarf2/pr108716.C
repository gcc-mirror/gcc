// PR debug/108716
// { dg-options "-O0 -gdwarf-5 -dA -fno-merge-debug-strings" }
// { dg-final { scan-assembler "DIE \\(\[^\n\r\]*\\) DW_TAG_imported_module\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_file\[^\n\r\]*\[\n\r]*\[^\n\r\]*0xc\[^\n\r\]* DW_AT_decl_line\[^\n\r\]*\[\n\r]*(\[^\n\r\]*0x13\[^\n\r\]* DW_AT_decl_column\[^\n\r\]*\[\n\r]*)?" } }

namespace M {
  int x = 1;
}

int
main ()
{
  using namespace M;
  return 0;
}
