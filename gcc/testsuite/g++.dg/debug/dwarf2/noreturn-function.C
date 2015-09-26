// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// Expect DW_AT_noreturn once in .debug_info and once in .debug_abbrev
// { dg-final { scan-assembler-times "DW_AT_noreturn" 2 { xfail { powerpc-ibm-aix* } } } }

class Foo
{
  int i;
  void bar [[noreturn]] (int j);
};

void
Foo::bar (int j)
{
  while (1) { ; }
}
