// PR 14804
// { dg-do run }

struct A {
  virtual void foo() = 0;
};

struct B : public A {
  virtual void bar() = 0;
};

typedef void (A::*mfptr)();

struct D {
  mfptr p;
};

static const D ds[] = {
  { reinterpret_cast<mfptr>(&B::bar) },
};

int main()
{
  return 0;
}
