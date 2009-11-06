// { dg-do compile }
struct f
{
  inline f(void);
  inline void f1(void);
  int a;
};

inline __attribute__((always_inline))  f::f(void)
{
  a++;
}

inline __attribute__((always_inline)) void  f::f1(void)
{
  a++;
}

void g(void)
{
  f a, b, c, d;
  a.f1();
}

// f::f() should be inlined even at -O0
// { dg-final { scan-assembler-not "_ZN1fC1Ev" } }
// Likewise for f::f1()
// { dg-final { scan-assembler-not "_ZN1f2f1Ev" } }
