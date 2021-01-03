// { dg-do run }
// { dg-options "-g" }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }

volatile int l;
int *volatile p;

namespace S
{
  int i = 24;
  void __attribute__((noinline))
  f()
  {
    int i = 42;
    l = i;		// { dg-final { gdb-test 15 "i" "42" } }
    {
      extern int i;
      p[0]++;
      l = i;		// { dg-final { gdb-test 19 "i" "24" } }
    }
  }
}

int
main (void)
{
  int x = 0;
  p = &x;
  S::f ();
  return 0;
}
