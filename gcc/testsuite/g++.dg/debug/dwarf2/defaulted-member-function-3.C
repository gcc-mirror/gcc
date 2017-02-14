// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-not " DW_AT_defaulted" { xfail { powerpc-ibm-aix* } } } }

struct Foo
{
};

void
bar ()
{
  Foo foo;
}
