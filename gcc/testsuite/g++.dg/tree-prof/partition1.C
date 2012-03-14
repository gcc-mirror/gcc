/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition" } */

struct A { A () __attribute__((noinline)); ~A () __attribute__((noinline)); };
A::A () { asm volatile ("" : : : "memory"); }
A::~A () { asm volatile ("" : : : "memory"); }

int bar () __attribute__((noinline));
void foo () __attribute__((noinline));

volatile int k, l;

int bar (int i)
{
  void *p = __builtin_alloca (i);
  asm volatile ("" : : "r" (i), "r" (p) : "memory");
  if (k) throw 6;
  return ++l;
}

void foo ()
{
  A a;
  try {
    A b;
    int i = bar (5);
    try { throw 6; } catch (int) {}
    if (__builtin_expect (i < 4500, 0)) {
      bar (7);
      try { bar (8); } catch (long) {}
      bar (10);
      if (__builtin_expect (i < 0, 0)) {
	try { bar (12); } catch (...) {}
	bar (16);
	bar (122);
      } else {
	try { bar (bar (7)); } catch (int) {}
      }
    } else {
      try { bar (bar (bar (9))); } catch (...) {}
      bar (5);
    }
  } catch (...) {
  }
}

int
main ()
{
  int i;
  for (i = 0; i < 10000; i++)
    foo ();
}
