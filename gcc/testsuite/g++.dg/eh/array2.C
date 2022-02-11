// Test that we clean up the right number of array elements when
// a temporary destructor throws.
// { dg-do run }

#if __cplusplus > 201100L
#define THROWING noexcept(false)
#else
#define THROWING
#endif

extern "C" void abort ();

int b;
int d = -1;
struct A {
  A() { }
  A(const A&);
  ~A() THROWING {
    if (b == d) throw b;
  }
};
struct B {
  B(const A& = A()) { ++b; }
  B(const B&);
  ~B() { --b; }
};
void f()
{
  b = 0;
  try
    {
      B bs[3];
      if (b != 3) abort ();
    }
  catch (int i) { }
  if (b != 0) abort ();
}

int main()
{
  for (d = 0; d <= 3; ++d)
    f();
}
