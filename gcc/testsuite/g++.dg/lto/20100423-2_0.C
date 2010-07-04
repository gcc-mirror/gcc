// { dg-lto-do assemble }
// { dg-lto-options {{-flto -g}} }

struct A
{
  virtual ~A();
};

void foo()
{
  struct B : A {};
  B b;
}

