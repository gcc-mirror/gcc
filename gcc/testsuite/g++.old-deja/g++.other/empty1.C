// Origin: Mark Mitchell <mark@codesourcery.com>
// This test case checks that the return value optimization works for
// empty classes.

// xfailed because empty classes clobbering what they overlay as the
// backend treats them as single byte objects. See bug 4222
// execution test - XFAIL *-*-*

extern "C" void abort();
extern "C" int printf (const char *, ...);

int i;

struct A;

struct A* as[10];

struct A {
  A () { as[i++] = this; }
  A (const A&) { as[i++] = this; }
  ~A() { if (i == 0 || as[--i] != this) abort(); }
};

A f() { return A(); }

int main ()
{
  A a (f ());
}
