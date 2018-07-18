// { dg-options "-gdwarf-2 -dA" }
// { dg-do compile }
// { dg-final { scan-assembler "DW_TAG_template_value_param" } }
// { dg-final { scan-assembler "N\[^\n\r]* DW_AT_name" } }
// { dg-final { scan-assembler "9\[^\n\r]* DW_AT_const_value" } }

template <int N> class C {};
template <typename T> struct E {};
E<struct A> f;
struct A { C<9> g; };
