// PR c++/48911
// { dg-do compile }
// { dg-options "-std=c++0x" }

#define SA(X) static_assert((X),#X)

struct A
{
  constexpr A () : a (6) {}
  int a;
};

int
main ()
{
  constexpr int a[2] = { 42 };
  constexpr int i = a[1];
  SA(i==0);
  constexpr int b[1] = { };
  constexpr int j = b[0];
  SA(j==0);
  constexpr char c[2] = "a";
  constexpr char k = c[1];
  SA(k==0);
  constexpr char d[2] = "";
  constexpr char l = d[1];
  SA(l==0);
  constexpr wchar_t e[2] = L"a";
  constexpr wchar_t m = e[1];
  SA(m==0);
  constexpr wchar_t f[2] = L"";
  constexpr wchar_t n = f[1];
  SA(n==0);
  constexpr A g[2] = { A () };
  constexpr A o = g[0];
  SA(o.a == 6);
  constexpr A p = g[1];
  SA(p.a == 6);
}
