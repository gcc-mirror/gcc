/* PR debug/49871 */
/* { dg-do compile } */
/* { dg-options "-gdwarf-3 -dA -fno-merge-debug-strings" } */

struct S
{
  char a[1 << 16];
  int b;
} s;

/* { dg-final { scan-assembler "\\(DW_AT_data_member_location\\)\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(DW_FORM_udata\\)" } } */
/* { dg-final { scan-assembler-not "\\(DW_AT_data_member_location\\)\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\\(DW_FORM_data\[48\]\\)" } } */
