// { dg-do compile { target c++11 } }
// { dg-options "-g -gno-strict-dwarf -dA" }
// { dg-final { scan-assembler-times " DW_AT_reference" 1 { xfail *-*-aix* } } }
// { dg-final { scan-assembler-times " DW_AT_rvalue_reference" 1 { xfail *-*-aix* } } }

struct S
{
  void foo ();
  void bar () &;
  void baz () &&;
};

void
test ()
{
  S s;
  s.foo ();
  s.bar ();
  S ().baz ();
}
