// PR rtl-optimization/36419
// { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-options "-Os -fasynchronous-unwind-tables -mpreferred-stack-boundary=4" }

extern "C" void abort ();

int v = -1;
unsigned int n;

__attribute__((noinline, used))
void foo (int a, int)
{
  if (v < 0)
    v = ((unsigned long) &a) & 15;
  else if ((((unsigned long) &a) & 15) != v)
    abort ();
  if (n++ == 0)
    throw 1;
}

__attribute__((noinline, used))
void baz (int a, int, int, int, int, int, int)
{
  if (v < 0)
    v = ((unsigned long) &a) & 15;
  else if ((((unsigned long) &a) & 15) != v)
    abort ();
  if (n++ == 0)
    throw 1;
}

struct A { A () { }; ~A (); char c[24]; };

__attribute__((noinline))
A::~A ()
{
  asm volatile ("" : : : "memory");
}

__attribute__((noinline))
void bar ()
{
  A a;
  try
    {
      foo (1, 2);
      baz (3, 4, 5, 6, 7, 8, 9);
    }
  catch (...)
    {
    }
  foo (1, 2);
  baz (3, 4, 5, 6, 7, 8, 9);
}

int
main ()
{
  bar ();
  return 0;
}
