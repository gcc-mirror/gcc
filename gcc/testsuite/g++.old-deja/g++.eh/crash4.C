// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Nathan Sidwell <nathan@codesourcery.com>

struct A
{
  A (int) { }
  ~A () { }
  int get () const { return 0; }
};

void f (const A &s) {
  f (s.get ());
}
