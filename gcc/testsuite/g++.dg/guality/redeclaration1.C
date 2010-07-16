// { dg-do run }
// { dg-options "-g" }

volatile int l;

namespace S
{
  int i = 24;
  void __attribute__((noinline))
  f()
  {
    int i = 42;
    l = i;		// { dg-final { gdb-test 13 "i" "42" } }
    {
      extern int i;
      l = i;		// { dg-final { gdb-test 16 "i" "24" } }
    }
  }
}

int
main (void)
{
  S::f ();
  return 0;
}
