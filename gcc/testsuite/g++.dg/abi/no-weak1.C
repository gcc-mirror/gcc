// { dg-options "-fno-weak" }
// { dg-final { scan-assembler "local\[ \t\]*_ZZL1fvE1i" { target i?86-*-gnu* x86_64-*-gnu* } } }
// { dg-final { scan-assembler "local\[ \t\]*_ZZ1gIiEvvE1i" { target i?86-*-gnu* x86_64-*-gnu* } } }

static inline void f()
{
  static int i;
  ++i;
};

template <class T> static void g()
{
  static int i;
  ++i;
}

int main()
{
  f();
  g<int>();
}
