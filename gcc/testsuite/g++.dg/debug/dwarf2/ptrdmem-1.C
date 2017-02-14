// { dg-do compile }
// { dg-options "-O -std=c++11 -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_ptr_to_member_type" 1 { xfail { powerpc-ibm-aix* } } } }
// { dg-final { scan-assembler-times " DW_AT_use_location" 1 { xfail { powerpc-ibm-aix* } } } }
// { dg-final { scan-assembler-not " DW_AT_reference" } }

struct S;
typedef int S::*pdm;
pdm pmf = 0;
