// { dg-do compile }
// { dg-options "-O -std=c99 -g -dA -gno-strict-dwarf" }
// Expect DW_AT_noreturn once in .debug_info and once in .debug_abbrev
// { dg-final { scan-assembler-times "DW_AT_noreturn" 2 } }

void __attribute__ ((noreturn))
baz (void)
{
  while (1) { ; }
}

