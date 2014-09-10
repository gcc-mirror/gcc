// { dg-options "-fno-weak" }
// { dg-final { scan-assembler "local\[ \t\]*_ZZL1fvE1i" { target x86_64-*-*gnu } } }

static inline void f()
{
  static int i;
  ++i;
};

int main()
{
  f();
}
