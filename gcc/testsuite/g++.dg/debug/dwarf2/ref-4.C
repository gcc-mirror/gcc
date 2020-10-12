// { dg-do compile { target c++11 } }
// { dg-options "-g -gno-strict-dwarf -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_typedef" 2 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_ptr_to_member_type" 2 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_subroutine_type" 1 } }

struct A { void foo (); int a; };
typedef void (A::*PMF) ();
typedef int A::*PMI;
PMF pmf;
PMI pmi;
