// PR debug/67664
// { dg-do compile }
// { dg-options "-gdwarf-2 -dA" }

struct T
{
  static const int a = 0;
};

int main()
{
  T t;
  return t.a;
}

// Once for the value and once for the abbrev
// { dg-final { scan-assembler-times "DW_AT_const_value" 2 } }
