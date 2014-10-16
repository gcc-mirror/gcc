/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf -fno-inline" } */

namespace {
struct A
{
  virtual void foo(void) {}
};
struct B: virtual A
{
  virtual void foo(void) {}
};
struct C: virtual A
{
  virtual void bar(void) {}
};
struct D: virtual A
{
  virtual void sparta(void) {}
};
struct E: B,C,D
{
  virtual void foo(void) {}
  virtual void barbar(void) {}
};
} // anonymous namespace

int main()
{
  struct A a;
  struct B b;
  struct C c;
  struct D d;
  struct E e;

  a.foo();
  b.foo();
  c.bar();
  d.foo();
  d.sparta();
  e.barbar();

  return 123;
}

/* { dg-final { scan-ipa-dump "Varpool alias has been created" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 2" "icf"  } } */
/* { dg-final { cleanup-ipa-dump "icf" } } */
