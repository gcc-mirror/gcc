// { dg-do compile { target c++11 } }
// { dg-options "-g -gno-strict-dwarf -dA" }
// { dg-final { scan-assembler-times " DW_AT_reference" 5 } }
// { dg-final { scan-assembler-times " DW_AT_rvalue_reference" 5 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_subroutine_type" 6 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_ptr_to_member_type" 7 } }
// { dg-final { scan-assembler-times " DW_AT_use_location" 1 } }

struct S
{
  void foo1 ();
  void bar1 () &;
  void baz1 () &&;
  void foo2 () const;
  void bar2 () const &;
  void baz2 () const &&;
  void foo3 () const;
  void bar3 () const &;
  void baz3 () const &&;
  int d;
};

void
test ()
{
  S s;
  auto o1 = &S::foo1;
  auto r1 = &S::bar1;
  auto z1 = &S::baz1;
  auto o2 = &S::foo2;
  auto r2 = &S::bar2;
  auto z2 = &S::baz2;
  auto o3 = &S::foo3;
  auto r3 = &S::bar3;
  auto z3 = &S::baz3;
  auto d1 = &S::d;
  void (S::*o4) () const;
  o4 = &S::foo3;
  void (S::*r4) () const &;
  r4 = &S::bar3;
  void (S::*z4) () const &&;
  z4 = &S::baz3;
  (s.*o1) ();
  (s.*r1) ();
  (S ().*z1) ();
  (s.*o2) ();
  (s.*r2) ();
  (S ().*z2) ();
  (s.*o3) ();
  (s.*r3) ();
  (S ().*z3) ();
  (s.*o4) ();
  (s.*r4) ();
  (S ().*z4) ();
  s.*d1 = 2;
}
