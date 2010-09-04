// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-std=c++0x -g -dA -fno-merge-debug-strings" }

// The type M<> should have one DW_TAG_GNU_template_parameter_pack DIE,
// with no DW_AT_name attribute. We don't test the fact that it has no
// DW_AT_name though.
// { dg-final { scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_GNU_template_parameter_pack" 1 } }


template <typename...>
struct M
{
};

struct R :
    M<>
{
};

R r;
