// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA" }
// { dg-final { scan-assembler-times " DW_AT_GNU_deleted" 2 } }
// { dg-additional-options "-gno-strict-dwarf" { target { *-*-darwin* } } }

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
