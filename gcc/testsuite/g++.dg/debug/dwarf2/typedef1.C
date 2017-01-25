// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40705
// { dg-options "-gdwarf-2 -dA -fno-debug-types-section" }
// { dg-do compile }
// { dg-final { scan-assembler-times "DW_TAG_structure_type" 2 } }
// { dg-final { scan-assembler-times "DW_AT_name: \"foo<1>\"|\"foo<1>..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "DW_TAG_enumeration_type" 2 } }
// { dg-final { scan-assembler-times "DW_AT_name: \"typedef foo<1>::type type\"|\"typedef foo<1>::type type..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_enumeration_type" 1 } }
// { dg-final { scan-assembler-times "\"e0..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"e1..\"\[^\n\]*DW_AT_name" 1 } }

template <unsigned int n>
struct foo
{
public:
    typedef
 unsigned char type;
};

template<>
struct foo<1>
{
    typedef enum { e0, e1 } type;
};

int
main()
{
    foo<1> f;
    foo<1>::type t = foo<1>::e1;
    return t;
}
