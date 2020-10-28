// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_ptr_to_member_type" 1 } }
// { dg-final { scan-assembler-times " DW_AT_rvalue_reference" 2 } }
// { dg-final { scan-assembler-not " DW_AT_use_location" } }
/* It is not clear what if anything we should output for
   DW_AT_use_location in a pointer to member function, so we don't
   output it for now.  */

struct S {
  void mf(void) &&;
};

void S::mf() && {}

typedef void (S::*pmft)(void) &&;
pmft pmf = &S::mf;
