// { dg-do compile }
// { dg-options "-O -std=c11 -g -dA -gno-strict-dwarf" }
// Expect DW_AT_noreturn once in .debug_info and once in .debug_abbrev
// { dg-final { scan-assembler-times "DW_AT_noreturn" 2 { xfail { powerpc-ibm-aix* } } } }

_Noreturn void exit (int);

void exit (int i)
{
  while (i < 0 || i == 0 || i > 0)
    ;
}

