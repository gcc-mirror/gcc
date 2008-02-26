/* Test that .debug_aranges does not have an entry for the text
   section if nothing went in there.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -ffunction-sections -w" } */
/* { dg-final { scan-assembler-not "\\.Letext0-\\.Ltext0" } } */

int
f (void)
{
  return 1;
}
