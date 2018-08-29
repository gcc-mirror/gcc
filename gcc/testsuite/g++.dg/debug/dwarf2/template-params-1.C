// Contributed by Dodji Seketeli <dodji@redhat.com>
// origin PR debug/30161
// { dg-options "-gdwarf-2 -dA" }
// { dg-do compile }
// { dg-final { scan-assembler "DW_TAG_template_type_param" } }
// { dg-final { scan-assembler "U\[^\n\r]* DW_AT_name" } }

template <class U>
class A
{
    U m;
};

A<int> a;

