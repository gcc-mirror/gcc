// Origin PR c++/47291
// { dg-options "-g -dA" }
// { dg-do compile }

struct S;
template< int S::* cst> struct D {};

struct S
{
  int i;
  D < &S::i > di; //<-- folding &S::i was failing
                  // because i has no offset as S is not laid out yet
};

int
main()
{
  S s;
  return s.i;
}

// { dg-final { scan-assembler-times "DIE \\(\[^\n\r\]*\\) DW_TAG_template_value_param" 1 } }
