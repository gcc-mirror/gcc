// PR debug/77363
// { dg-options "-gdwarf-2 -dA -fno-merge-debug-strings" }
// { dg-final { scan-assembler "DIE \\(\[^\n\r\]*\\) DW_TAG_typedef\[^\n\r\]*\[\n\r]*\[^\n\r\]*type2\[^\n\r\]* DW_AT_name\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_file\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_line\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_type" } }
// { dg-final { scan-assembler "DIE \\(\[^\n\r\]*\\) DW_TAG_typedef\[^\n\r\]*\[\n\r]*\[^\n\r\]*type3\[^\n\r\]* DW_AT_name\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_file\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_line\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_type" } }
// { dg-final { scan-assembler "DIE \\(\[^\n\r\]*\\) DW_TAG_typedef\[^\n\r\]*\[\n\r]*\[^\n\r\]*type4\[^\n\r\]* DW_AT_name\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_file\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_line\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_type" } }
// { dg-final { scan-assembler "DIE \\(\[^\n\r\]*\\) DW_TAG_typedef\[^\n\r\]*\[\n\r]*\[^\n\r\]*type5\[^\n\r\]* DW_AT_name\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_file\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_decl_line\[^\n\r\]*\[\n\r]*\[^\n\r\]* DW_AT_type" } }

typedef unsigned short type1;
typedef unsigned char type2;
typedef type2 type3[16];
typedef unsigned char type4[16];
typedef struct
{
  struct
  {
    type3 a;
    type4 b;
  } c;
} type5;
type5 var;
