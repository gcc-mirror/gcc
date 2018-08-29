// { dg-options "-fpermissive -w" }

struct A
{
  template<int> void foo()
  {
    void (A::* fp)();
    fp = A::foo<0>;  // { dg-bogus "pointer to member" }
  }
};

void bar()
{
  A().foo<0>();
}
