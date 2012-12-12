// PR debug/55655
// { dg-do run }
// { dg-options "-g" }

extern "C" void abort ();
struct A { A (int); int a; };

__attribute__((noinline, noclone)) int
bar (void)
{
  return 40;
}

__attribute__((noinline, noclone)) void
foo (int x)
{
  __asm volatile ("" : : "r" (x) : "memory");
}

A::A (int x)
{
  static int p = bar ();
  foo (p);		// { dg-final { gdb-test 23 "p" "40" } }
  a = ++p;
}

int
main ()
{
  A a (42);
  if (a.a != 41)
    abort ();
}
