// PR c++/54410
// { dg-options "-g -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_type_param" 1 } }

namespace N {
  template <class T> struct A { };
}

N::A<int> a;
