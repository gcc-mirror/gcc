// { dg-do assemble  }
// Bug: g++ can't deal with multi-language overloading.

extern void foo (int, int);
extern "C" void foo (int);

void bar ()
{
  foo (1);
  foo (1, 2);
}
