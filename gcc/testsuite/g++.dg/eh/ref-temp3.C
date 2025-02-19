// { dg-do run }

extern "C" void abort();

int a;

struct A {
  A()
  {
    if (a == 1)
      throw 42;
    ++a;
  }
  ~A()
  {
    --a;
  }
};

struct B {
  const A &a1;
  const A &a2;
};

void f();

int main()
{
  try
    {
      B b = { A(), A() };
    }
  catch (...) { }
  if (a != 0)
    abort ();
}
