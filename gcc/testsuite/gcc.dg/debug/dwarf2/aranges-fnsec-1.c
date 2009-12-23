/* Test that .debug_aranges and .debug_ranges do not have an entry for the
   text section if nothing went in there.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -ffunction-sections -w -dA" } */
/* { dg-final { scan-assembler-not "\\.Letext0-\\.Ltext0" } } */
/* { dg-final { scan-assembler-not "\\.Ltext0\[^\n\r\]*Offset 0x0" } } */
/* { dg-final { scan-assembler "DW_AT_ranges" } } */

int
f (void)
{
  return 1;
}
