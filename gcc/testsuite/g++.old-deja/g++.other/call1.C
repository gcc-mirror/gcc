// Test that various calls to non-functions work.

void f () { }

typedef void (*fptr)();
typedef void (&fref)();
fptr p = f;
fref r = f;
const fptr &pr = p;

struct A {
  fptr p;

  A (fptr n): p(n) { }
  operator fptr () { return p; }
};

struct B {
  fref r;

  B (fptr n): r(*n) { }
  operator const fref () { return r; }
};

struct C {
  const fptr pr;

  C (fptr n): pr(n) { }
  operator const fptr& () { return pr; }
};

int main ()
{
  f();

  p();
  r();
  pr();

  A a (f);
  a();
  a.p();

  B b (f);
  b();
  b.r();

  C c (f);
  c();
  c.pr();
}
