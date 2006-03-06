// PR c++/15759
// Origin:   Lars Rune Nøstdal  <larsnostdal@gmail.com>
// Testcase: Volker Reichelt    <reichelt@igpm.rwth-aachen.de>

// { dg-do run }

extern "C" void abort();

int n = 0;

int f() { return ++n; }

int(&foo1)() = f;
int(*foo2)() = &f;
int(*foo3)() = f;

int bar1(int i = foo1()) { return i; }
int bar2(int i = foo2()) { return i; }
int bar3(int i = foo3()) { return i; }
int bar4(int i = f())    { return i; }

int main()
{
  if (bar1() != 1) abort();
  if (bar2() != 2) abort();
  if (bar3() != 3) abort();
  if (bar4() != 4) abort();
  return 0;
}
