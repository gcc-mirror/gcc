// { dg-do compile }
// { dg-options "-gdwarf-2 -dA -fno-merge-debug-strings -fno-debug-types-section" }

struct A
{
  static int staticdatamember;
};

int A::staticdatamember = 6;

// { dg-final { scan-assembler "DW_TAG_member\[^\n\r\]*\[\n\r\]*\[^\n\r\]*staticdatamember\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "DW_TAG_variable\[^\n\r\]*\[\n\r\]*\[^\n\r\]*DW_AT_specification" } }
