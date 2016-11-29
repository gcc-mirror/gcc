// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times " DW_AT_deleted" 2 { xfail { powerpc-ibm-aix* } } } }

struct Foo
{
  Foo () {}
  // Make non-copyable
  Foo (const Foo&) = delete;
  Foo & operator=(const Foo&) = delete;
};

void
bar ()
{
  Foo foo;
}
