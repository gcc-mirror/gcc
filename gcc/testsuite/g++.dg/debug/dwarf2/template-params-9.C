// Origin PR debug/PR46973
// { dg-options "-g -dA" }
// { dg-do compile }

struct S
{
  int f;
};

template<typename T, int I, int *P, int S::*MP>
struct Base
{
  template<typename Z>
  struct Inner
  {
  };
};

int a_global;

int main ()
{
  Base<long, 47, &a_global, &S::f>::Inner<float> inner;
  return 0;
}

// { dg-final { scan-assembler-times "DIE \\(\[^\n\r\]*\\) DW_TAG_template_type_param" 2 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\r\]*\\) DW_TAG_template_value_param" 3 } }
