// { dg-do compile }
// { dg-options "-Wuninitialized" }

struct A;
struct B
{
  B(A);
};
struct C
{
  template <typename PassT> void m_fn1(PassT p1) { new B(p1); }  // { dg-bogus "uninitialized" }
};
struct A {};
void fn1()
{
  C a;
  a.m_fn1(A());
}
