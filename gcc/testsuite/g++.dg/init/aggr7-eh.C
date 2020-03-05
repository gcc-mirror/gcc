// PR c++/50866, adjusted
// { dg-do run }

#if __cplusplus > 201100L
#define THROWING noexcept(false)
#else
#define THROWING
#endif

extern "C" void abort ();

int a;
int d = -1;
struct A {
  A() { ++a; }
  A(const A&);
  ~A() THROWING {
    --a;
    if (a == d)
      throw (short)a;
  }
};
int b;
int t;
struct B {
  B(const char *, const A& = A())
  {
    if (b == t)
      throw b;
    ++b;
    if (a != b) abort ();
  }
  B(const B&);
  ~B()
  {
    --b;
  }
};
struct C {
  B b1, b2, b3;
};
void f()
{
  try
    {
      C c = { "a","b","c" };
      if (a != 0) abort ();
      if (b != 3) abort ();
    }
  catch (int i) { }
  catch (short s) { }
  if (a != 0) abort ();
  if (b != 0) abort ();
}

int main()
{
  for (t = 0; t <= 3; ++t)
    f();
  for (d = 0; d <= 2; ++d)
    f();
}
