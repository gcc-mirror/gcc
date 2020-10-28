// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times "0x2\[ \t\]\[^\n\]* DW_AT_defaulted" 1 } }

struct Foo
{
  Foo ();
};

Foo::Foo () = default;

void
bar ()
{
  Foo foo;
}
