// { dg-do compile }

extern "C" {
void __assert_fail ();
__SIZE_TYPE__ strlen (const char *);
}
class A
{
  int Data;
  int Length;

public:
  A (const char *p1) : Data ()
  {
    p1 ? void() : __assert_fail ();
    Length = strlen (p1);
  }
};
enum TokenKind
{
  semi
};
class B
{
public:
  void m_fn1 ();
};
class C
{
  void m_fn2 (TokenKind, int, A);
  struct D
  {
    D (int);
    B Range;
  };
  int *m_fn3 (const int &, int &, int **);
};
int a, b;
int *
C::m_fn3 (const int &, int &, int **)
{
  D c (0);
  if (a)
    c.Range.m_fn1 ();
  m_fn2 (semi, 0, b ? "" : a ? "alias declaration" : "using declaration");
}
