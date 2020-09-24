/* PR tree-optimization/96979 */
/* { dg-do compile } */
/* { dg-options "-std=c++17 -O2" } */

using u64 = unsigned long long;

constexpr inline u64
foo (const char *str) noexcept
{
  u64 value = 0xcbf29ce484222325ULL;
  for (u64 i = 0; str[i]; i++)
    value = (value ^ u64(str[i])) * 0x100000001b3ULL;
  return value;
}

struct V
{
  enum W
  {
#define A(n) n,
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
#define D(n) C(n##0) C(n##1) C(n##2) C(n##3) C(n##4) C(n##5) C(n##6) C(n##7) C(n##8) C(n##9)
#define E D(foo1) D(foo2) D(foo3)
    E
    last
  };

  constexpr static W
  bar (const u64 h) noexcept
  {
    switch (h)
      {
#undef A
#define F(n) #n
#define A(n) case foo (F(n)): return n;
        E
      }
    return last;
  }
};

int
baz (const char *s)
{
  const u64 h = foo (s);
  return V::bar (h);
}
